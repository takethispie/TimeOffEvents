namespace TimeOff

open System
open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid 
    | RequestActiveTimeOffList of TimeOffRequest
    | UpdateRequest of TimeOffRequest * Guid
    | CancelRequest of UserId * Guid
    | RefuseRequest of UserId * Guid
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | UpdateRequest (newRequest,oldRequestId) -> newRequest.UserId
        | CancelRequest (userId, _) -> userId
        | RefuseRequest (userId, _) -> userId


type Query =
    | GetAllActive of UserId
    member this.UserId =
        match this with
        | GetAllActive userId -> userId


// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest 
    | RequestUpdated of TimeOffRequest 
    | RequestRefused of TimeOffRequest
    | RequestPendingCancellation of TimeOffRequest
    | RequestCanceled of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestPendingCancellation request -> request
        | RequestCanceled request -> request
        | RequestRefused request -> request
        | RequestUpdated request ->request


// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | Refused of TimeOffRequest
        | PendingCancellation of TimeOffRequest
        | CancellationRefused of TimeOffRequest
        | Canceled of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Refused request -> request
            | Canceled request -> request
            | PendingCancellation request -> request
            | CancellationRefused request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _ ->true
            | Validated _ -> true
            | Refused _ -> false
            | Canceled _ -> false
            | PendingCancellation _ -> true
            | CancellationRefused _ -> true
            

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestCanceled request -> Canceled request
        | RequestRefused request -> Refused request
        | RequestPendingCancellation request -> PendingCancellation request
        | RequestUpdated request -> PendingValidation request


    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)


    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest) =
        if ((request1.End.Date < request2.Start.Date) || (request1.Start.Date > request2.End.Date) ) then false
        else not (request1.Start.HalfDay <> request2.Start.HalfDay && request1.End.HalfDay <> request1.End.HalfDay)
        //TODO: check haldfay within the compare in the if clause 


    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        let boolMap = Seq.map (fun otherReq -> overlapsWith otherReq request) otherRequests
        Seq.exists (id) boolMap


    let createRequest activeUserRequests (currentDate: DateTime)  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= currentDate then
            Error "The request starts in the past"
        else
            if request.Start.Date < currentDate then
                Error "Request start date is in the past"
            else
                Ok [RequestCreated request]


    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"


    let CancelRequest requestState (currentDate: DateTime) userId =
        
        if userId <> Manager then 
            match requestState with 
            | Validated request -> 
                if request.Start.Date > currentDate.AddDays(1.) then 
                    Ok [RequestCanceled request]
                else Ok [RequestPendingCancellation request]
            | PendingValidation request -> 
                if request.Start.Date > currentDate.AddDays(1.) then 
                    Ok [RequestCanceled request]
                else Ok [RequestPendingCancellation request]
            | _ -> Error "Request cannot be deleted"
        else 
            match requestState with
            | Validated request -> 
               Ok [RequestCanceled request]
            | PendingValidation request -> 
               Ok [RequestCanceled request]
            | PendingCancellation request -> 
               Ok [RequestCanceled request]
            | CancellationRefused request -> 
               Ok [RequestCanceled request]
            | _ -> Error "Request cannot be canceled"   
                     

    let RefuseRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | Validated request -> 
            Ok [RequestRefused request]
        | PendingCancellation request -> 
            Ok [RequestRefused request]
        | _ -> Error "Cannot refuse request"

    let UpdateRequest userRequests newRequest  oldRequestId =
        try
            let oldRequest = userRequests//seul les requete pending ou validé peuvent etre modifié. une requete validé, un fois modifiée, repasse en pending
                            |> Map.toSeq
                            |> Seq.map (fun (_, state) -> state)//retourne les RequestState
                            |> Seq.where (fun (state:RequestState) -> state.IsActive)//ne prend que ceux qui sont actif
                            |> Seq.where (fun (state)->state.Request.Start.Date>DateTime.Now)//et dont la date de début et posterieur à aujourd'hui
                            |> Seq.map (fun state -> state.Request) //extrait les requete des RequestState                       
                            |> Seq.where(fun request -> request.RequestId.Equals(oldRequestId) )//ne prend que celle qui as le bon ID
                            |> Seq.exactlyOne //transforme la sequence unique ne requete
 
            //on creer la requete à partir des info de l'ancienne et de la nouvelle
            let result=PendingValidation{
                UserId=oldRequest.UserId
                RequestId=oldRequest.RequestId
                Start=newRequest.Start
                End=newRequest.End
            }
            
            Ok[RequestUpdated result.Request]
        with 
            | :? _-> Error "Time off in the past or already cancelled"

    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)
                createRequest activeUserRequests DateTime.Today request

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState

            | CancelRequest (_, requestId) -> 
                let requestStat = defaultArg (userRequests.TryFind requestId) NotCreated
                CancelRequest requestStat DateTime.Today user

            | RefuseRequest(_, requestId) -> 
                if user <> Manager then 
                    Error "Unauthorized"
                else 
                    let requestStat = defaultArg (userRequests.TryFind requestId) NotCreated
                    RefuseRequest requestStat

            | UpdateRequest (newRequest,oldRequestId) ->
                
                UpdateRequest userRequests  newRequest oldRequestId
              


    let fetch (userRequests: UserRequestsState) (user: User) (query: Query) =
        match query with
        |GetAllActive userId-> 
            let result = 
                userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive && state.Request.UserId = userId)
                    |> Seq.map (fun state -> state.Request)
            result

