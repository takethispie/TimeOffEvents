module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command
    Expect.equal result expected message


let whenQuery (query: Query) (events: RequestEvent list, user: User) = events, user, query
let thenQuery expected message  (events: RequestEvent list, user: User, query: Query) =
  let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

  let globalState = Seq.fold evolveGlobalState Map.empty events
  let userRequestsState = defaultArg (Map.tryFind query.UserId globalState) Map.empty
  let result = Logic.fetch userRequestsState user query
  Expect.equal result expected message

open TimeOff

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
  ]


[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 31); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 31); HalfDay = PM } 
      }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      //teste que la request n'overlap pas (utilise overlapsWithAnyRequest)
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "test " {
      let request1 = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }

      let request3 = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 29); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 29); HalfDay = PM } 
      }

      let request2 = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 3, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 3, 28); HalfDay = PM } 
      }

      let request4 = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 11, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 11, 28); HalfDay = PM } 
      }

      let other = Seq.ofList [request1; request2; request3]
      //request is used in both in the list and the request 
      //the function is tested against
      let res = Logic.overlapsWithAnyRequest other request1
      Expect.isTrue(res) "two same request should overlap with each other"
    }
  ]


[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> When (ValidateRequest ("jdoe", Guid.Empty))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]


[<Tests>]
let DeletionTests = 
  testList "DeletionTests" [
    test "a request is deleted" {
      let request = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }

      Given [ RequestValidated request]
      |> ConnectedAs Manager
      |> When ( CancelRequest ("1", Guid.Empty))
      |> Then (Ok [RequestCanceled request]) "the request should have been deleted"
    }

    test "a request is in the past thus not deleted" {
      let request = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 2); HalfDay = PM } 
      }

      Given [ RequestValidated request]
      |> ConnectedAs (Employee "1")
      |> When ( CancelRequest ("1", Guid.Empty))
      |> Then ( Ok [RequestPendingCancellation request]) "the request is waiting for cancellation from the manager"
    }
  ]


[<Tests>]
let UpdateTest = 
  testList "UpdateTests" [
    test "a request is updated" {
      let request = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }
      let newRequest = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }


      Given [ RequestValidated request]
      |> ConnectedAs (Employee "1")
      |> When ( UpdateRequest (request,newRequest.RequestId))
      |> Then (Ok [RequestUpdated newRequest]) "the request should have been updated"
    }
    test "a request refuses to update due to it beeing canceled" {
      let request = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }
      let newRequest = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }


      Given [ RequestCanceled request]
      |> ConnectedAs (Employee "1")
      |> When ( UpdateRequest (request,newRequest.RequestId))
      |> Then (Error "Time off in the past or already cancelled") "the request should not have been updated"
    }
    test "a request refuses to update due to it beeing in the past" {
      let request = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } 
      }
      let newRequest = {
        UserId = "1"
        RequestId = Guid.Empty
        Start = { Date = DateTime(2019, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 28); HalfDay = PM } 
      }


      Given [ RequestValidated request]
      |> ConnectedAs (Employee "1")
      |> When ( UpdateRequest (request,newRequest.RequestId))
      |> Then (Error "Time off in the past or already cancelled") "the request should not have been updated"
    }
 
  ]

// [<Tests>]
// let QueriesTests = 
//   testList "queries test" [
//     test "get all active request" {
//       let request3: TimeOffRequest = {
//         UserId = 1
//         RequestId = Guid.Empty
//         Start = { Date = DateTime(2019, 12, 29); HalfDay = AM }
//         End = { Date = DateTime(2019, 12, 29); HalfDay = PM } 
//       }

//       let request2: TimeOffRequest = {
//         UserId = 1
//         RequestId = Guid.Empty
//         Start = { Date = DateTime(2019, 3, 28); HalfDay = AM }
//         End = { Date = DateTime(2019, 3, 28); HalfDay = PM } 
//       }

//       let other = List.toSeq [request3; request2;]

//       Given[RequestCreated request2; RequestCreated request3;]
//       |> ConnectedAs (Employee 1)
//       |> whenQuery ( GetAllActive (1))
//       |> thenQuery (other) "lol"
//     }
//   ]
