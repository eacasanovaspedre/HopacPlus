module HopacPlus.Tests.BoundedMbTests

open Expecto
open HopacPlus
open Helpers

[<Tests>]
let tests =
    testList
        "BoundedMb"
        [ testCase "put and take"
          <| fun () ->
              let mb = BoundedMb.create 2

              eq
                  1
                  (run (
                      job {
                          do! BoundedMb.put mb 1
                          return! BoundedMb.take mb
                      }
                  )) ]
