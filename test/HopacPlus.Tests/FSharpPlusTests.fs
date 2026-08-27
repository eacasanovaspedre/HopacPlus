module HopacPlus.Tests.FSharpPlusTests

open Expecto
open FSharpPlus
open HopacPlus
open Helpers

[<Tests>]
let tests =
    testList
        "FSharpPlus"
        [ testCase "Job map bind monad"
          <| fun () ->
              eq 2 (run (map ((+) 1) (Job.result 1)))
              eq 2 (run (Job.result 1 >>= fun x -> Job.result (x + 1)))

              let computed =
                  monad {
                      let! x = Job.result 1
                      let! y = Job.result 2
                      return x + y
                  }

              eq 3 (run computed)

          testCase "monad over Alt"
          <| fun () ->
              let computed =
                  monad {
                      let! x = Alt.always 1
                      let! y = Alt.always 2
                      return x + y
                  }

              eq 3 (run computed)

          testCase "Alt map and Empty"
          <| fun () ->
              eq 2 (run (map ((+) 1) (Alt.always 1)))

              eq
                  1
                  (run (
                      Alt.choose
                          [ Alt.Empty ()
                            Alt.always 1 ]
                  )) ]
