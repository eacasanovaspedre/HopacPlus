module HopacPlus.Tests.PromiseTests

open Expecto
open HopacPlus
open Helpers

[<Tests>]
let tests =
    testList
        "Promise"
        [ testCase "Now.withValue and read"
          <| fun () -> eq 1 (run (Promise.read (Promise.Now.withValue 1)))

          testCase "Now.get and isFulfilled"
          <| fun () ->
              let p = Promise.Now.withValue 2
              Expect.isTrue (Promise.Now.isFulfilled p) ""
              eq 2 (Promise.Now.get p)

          testCase "start and queue"
          <| fun () ->
              eq
                  3
                  (run (
                      job {
                          let! p = Promise.start (Job.result 3)
                          return! Promise.read p
                      }
                  ))

              eq
                  4
                  (run (
                      job {
                          let! p = Promise.queue (Job.result 4)
                          return! Promise.read p
                      }
                  ))

          testCase "Now.delay"
          <| fun () -> eq 5 (run (Promise.read (Promise.Now.delay (Job.result 5))))

          testCase "let! on a promise"
          <| fun () ->
              eq
                  6
                  (run (
                      job {
                          let! x = Promise.Now.withValue 6
                          return x
                      }
                  ))

          testCase "Now.never is not fulfilled"
          <| fun () -> Expect.isFalse (Promise.Now.isFulfilled (Promise.Now.never ())) ""

          testCase "Now.withFailure"
          <| fun () -> throws (Promise.read (Promise.Now.withFailure (TestExn "e"))) ]
