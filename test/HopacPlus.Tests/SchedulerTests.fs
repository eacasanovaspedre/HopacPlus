module HopacPlus.Tests.SchedulerTests

open Expecto
open HopacPlus
open Helpers

[<Tests>]
let tests =
    testList
        "Scheduler"
        [ testCase "run on a local scheduler"
          <| fun () ->
              let sr = Scheduler.create Hopac.Scheduler.Create.Def
              eq 1 (Scheduler.run sr (Job.result 1)) ]
