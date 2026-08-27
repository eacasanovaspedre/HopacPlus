module HopacPlus.Tests.ExtensionTests

open Expecto
open HopacPlus
open HopacPlus.Extensions
open Helpers

[<Tests>]
let tests =
    testList
        "Extensions"
        [ testCase "Seq.mapJob and iterJob"
          <| fun () ->
              let xs =
                  run (
                      Seq.mapJob
                          Job.result
                          [ 1
                            2
                            3 ]
                  )

              eq
                  [ 1
                    2
                    3 ]
                  (List.ofSeq xs)

              let acc = ref 0

              run (
                  Seq.iterJob
                      (fun i -> Job.thunk (fun () -> acc := acc.Value + i))
                      [ 1
                        2 ]
              )

              eq 3 acc.Value

          testCase "Array.mapJob"
          <| fun () ->
              eq
                  [| 2
                     3 |]
                  (run (
                      Array.mapJob
                          (fun x -> Job.result (x + 1))
                          [| 1
                             2 |]
                  ))

          testCase "Async.toJob"
          <| fun () -> eq 4 (run (Async.toJob (async { return 4 })))

          testCase "Seq.foldJob and Con.mapJob"
          <| fun () ->
              eq
                  6
                  (run (
                      Seq.foldJob
                          (fun acc x -> Job.result (acc + x))
                          0
                          [ 1
                            2
                            3 ]
                  ))

              let xs =
                  run (
                      Seq.Con.mapJob
                          Job.result
                          [ 1
                            2
                            3 ]
                  )

              Expect.containsAll
                  (List.ofSeq xs)
                  [ 1
                    2
                    3 ]
                  ""

          testCase "Array.iterJob"
          <| fun () ->
              let acc = ref 0

              run (
                  Array.iterJob
                      (fun i -> Job.thunk (fun () -> acc := acc.Value + i))
                      [| 1
                         2
                         3 |]
              )

              eq 6 acc.Value

          testCase "Async.Global.ofJob"
          <| fun () -> eq 5 (Async.RunSynchronously (Async.Global.ofJob (Job.result 5)))

          testCase "Task.startJob"
          <| fun () ->
              eq
                  6
                  (run (
                      job {
                          let! t = Task.startJob (Job.result 6)
                          return! Job.awaitTask t
                      }
                  )) ]
