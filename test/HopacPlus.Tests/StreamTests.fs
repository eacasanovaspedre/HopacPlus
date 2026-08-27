module HopacPlus.Tests.StreamTests

open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "Stream"
        [ testCase "one ofSeq mapFun take toSeq"
          <| fun () ->
              let xs =
                  Stream.ofSeq
                      [ 1
                        2
                        3
                        4 ]

              let ys = Stream.mapFun ((+) 1) xs
              let zs = Stream.take 2 ys

              eq
                  [ 2
                    3 ]
                  (List.ofSeq (run (Stream.toSeq zs)))

          testCase "nil and one"
          <| fun () ->
              eq [] (List.ofSeq (run (Stream.toSeq Stream.nil)))
              eq [ 7 ] (List.ofSeq (run (Stream.toSeq (Stream.one 7))))

          testCase "stream builder"
          <| fun () ->
              let xs =
                  stream {
                      yield 1
                      yield 2
                  }

              eq
                  [ 1
                    2 ]
                  (List.ofSeq (run (Stream.toSeq xs)))

          testCase "head last tail"
          <| fun () ->
              let xs =
                  Stream.ofSeq
                      [ 1
                        2
                        3 ]

              eq [ 1 ] (List.ofSeq (run (Stream.toSeq (Stream.head xs))))
              eq [ 3 ] (List.ofSeq (run (Stream.toSeq (Stream.last xs))))

              eq
                  [ 2
                    3 ]
                  (List.ofSeq (run (Stream.toSeq (Stream.tail xs))))

          testCase "appendAll of wrapped streams"
          <| fun () ->
              let xss =
                  Stream.ofSeq
                      [ Stream.ofSeq
                            [ 1
                              2 ]
                        Stream.ofSeq [ 3 ] ]

              eq
                  [ 1
                    2
                    3 ]
                  (List.ofSeq (run (Stream.toSeq (Stream.appendAll xss))))

          testCase "Src value close tap"
          <| fun () ->
              let src = Stream.Src.create ()
              let xs = Stream.Src.tap src

              eq
                  [ 1
                    2 ]
                  (run (
                      job {
                          do! Stream.Src.value src 1
                          do! Stream.Src.value src 2
                          do! Stream.Src.close src
                          let! ys = Stream.toSeq xs
                          return List.ofSeq ys
                      }
                  ))

          testCase "Var get set tap"
          <| fun () ->
              let v = Stream.Var.create 1
              eq 1 (Stream.Var.get v)
              run (Stream.Var.set v 2)
              eq 2 (Stream.Var.get v)
              eq [ 2 ] (List.ofSeq (run (Stream.toSeq (Stream.take 1 (Stream.Var.tap v)))))

          testCase "MVar get set"
          <| fun () ->
              let m = Stream.MVar.create 3
              eq 3 (run (Stream.MVar.get m))
              run (Stream.MVar.set m 4)
              eq 4 (run (Stream.MVar.get m))

          testCase "iterFun"
          <| fun () ->
              let acc = ref 0
              run (Stream.iterFun (fun x -> acc := x) (Stream.one 8))
              eq 8 acc.Value

          testCase "values" <| fun () -> eq 9 (run (Stream.values (Stream.one 9))) ]
