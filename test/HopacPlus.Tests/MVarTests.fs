module HopacPlus.Tests.MVarTests

open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "MVar"
        [ testCase "fill take read"
          <| fun () ->
              let mv = MVar.create ()

              eq
                  2
                  (run (
                      job {
                          do! MVar.fill mv 1
                          let! x = MVar.read mv
                          let! y = MVar.take mv
                          return x + y
                      }
                  ))

          testCase "createFull and mutateFun"
          <| fun () ->
              let mv = MVar.createFull 2

              eq
                  3
                  (run (
                      job {
                          do! MVar.mutateFun ((+) 1) mv
                          return! MVar.read mv
                      }
                  ))

          testCase "infix fill"
          <| fun () ->
              let mv = MVar.create ()

              eq
                  4
                  (run (
                      job {
                          do! mv *<<= 4
                          return! mv
                      }
                  ))

          testCase "modifyFun"
          <| fun () ->
              let mv = MVar.createFull 1
              eq 10 (run (MVar.modifyFun (fun x -> (x + 1, x * 10)) mv))
              eq 2 (run (MVar.read mv)) ]
