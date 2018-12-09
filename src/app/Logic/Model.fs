namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid 
    | DeleteRequest of UserId * Guid
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | DeleteRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest 
    | RequestDeleted of TimeOffRequest with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestDeleted request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest 
        | Deleted of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request -> request
            | Deleted request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | Validated _ -> true
            | Deleted _ -> false
            
            

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
        | RequestValidated request -> Validated request
        | RequestDeleted request -> Deleted request

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
        Seq.exists (fun b -> b = true) boolMap

    let createRequest activeUserRequests (currentDate: DateTime)  request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= currentDate then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

    let DeleteRequest requestState (currentDate: DateTime) =
        match requestState with 
        | Validated request -> 
            if request.End.Date < currentDate then Error "Can't delete passed Request"
            else Ok [RequestDeleted request]
        | _ -> Error "Request cannot be deleted"


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

            | DeleteRequest (_, requestId) -> 
                let requestStat = defaultArg (userRequests.TryFind requestId) NotCreated
                DeleteRequest requestStat DateTime.Today
