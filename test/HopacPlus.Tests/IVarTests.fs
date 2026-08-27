module HopacPlus.Tests.IVarTests

open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "IVar"
        [ testCase "fill and read"
          <| fun () ->
              let iv = IVar.create ()

              eq
                  1
                  (run (
                      job {
                          do! IVar.fill iv 1
                          return! IVar.read iv
                      }
                  ))

          testCase "tryFill does not fail twice"
          <| fun () ->
              let iv = IVar.create ()

              eq
                  2
                  (run (
                      job {
                          do! IVar.tryFill iv 2
                          do! IVar.tryFill iv 99
                          return! iv
                      }
                  ))

          testCase "fillFailure"
          <| fun () ->
              let iv = IVar.create ()

              throws (
                  job {
                      do! IVar.fillFailure iv (TestExn "e")
                      return! IVar.read iv
                  }
              )

          testCase "Now.isFull"
          <| fun () ->
              let iv = IVar.create ()
              Expect.isFalse (IVar.Now.isFull iv) ""
              run (iv *<= 3)
              Expect.isTrue (IVar.Now.isFull iv) ""
              eq 3 (IVar.Now.get iv)

          testCase "createFull" <| fun () -> eq 9 (run (IVar.read (IVar.createFull 9)))

          testCase "createFailure"
          <| fun () -> throws (IVar.read (IVar.createFailure (TestExn "e")))

          testCase "tryFillFailure does not overwrite a value"
          <| fun () ->
              let iv = IVar.create ()

              eq
                  1
                  (run (
                      job {
                          do! IVar.tryFill iv 1
                          do! IVar.tryFillFailure iv (TestExn "no")
                          return! iv
                      }
                  )) ]
