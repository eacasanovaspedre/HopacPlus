module HopacPlus.Tests.LockTests

open Expecto
open HopacPlus
open Helpers

[<Tests>]
let tests =
    testList
        "Lock"
        [ testCase "duringFun"
          <| fun () ->
              let l = Lock.create ()
              eq 1 (run (Lock.duringFun l (fun () -> 1)))

          testCase "duringJob"
          <| fun () ->
              let l = Lock.create ()
              eq 2 (run (Lock.duringJob l (Job.result 2))) ]
