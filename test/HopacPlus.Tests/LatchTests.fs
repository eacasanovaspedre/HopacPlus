module HopacPlus.Tests.LatchTests

open Expecto
open HopacPlus
open Helpers

[<Tests>]
let tests =
    testList
        "Latch"
        [ testCase "create 1, decrement, await"
          <| fun () ->
              eq
                  ()
                  (run (
                      job {
                          let l = Latch.create 1
                          do! Latch.decrement l
                          return! Latch.await l
                      }
                  ))

          testCase "within" <| fun () -> eq 1 (run (Latch.within (fun _ -> Job.result 1)))

          testCase "holding"
          <| fun () -> eq 2 (run (Latch.holding (Latch.create 1) (Job.result 2)))

          testCase "queue then decrement to open"
          <| fun () ->
              eq
                  1
                  (run (
                      job {
                          let l = Latch.create 1
                          let! p = Latch.queueAsPromise l (Job.result 1)
                          do! Latch.decrement l
                          do! Latch.await l
                          return! Promise.read p
                      }
                  )) ]
