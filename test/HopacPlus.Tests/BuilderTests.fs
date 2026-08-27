module HopacPlus.Tests.BuilderTests

open System
open System.Threading.Tasks
open Expecto
open HopacPlus
open Helpers

// SRTP: let! Alt works via Job.bind/ToHopac. return! Alt needed JobBuilder.ReturnFrom(Alt).

[<Tests>]
let tests =
    testList
        "job builder"
        [ testCase "let! binds Job, Alt, Async, Task and unit Task"
          <| fun () ->
              let result =
                  job {
                      let! a = Job.result 1
                      let! b = Alt.always 2
                      let! c = async { return 3 }
                      let! d = Task.FromResult 4
                      do! Task.CompletedTask
                      return a + b + c + d
                  }
                  |> run

              eq 10 result

          testCase "return! Job" <| fun () -> eq 7 (run (job { return! Job.result 7 }))

          testCase "return! Alt" <| fun () -> eq 8 (run (job { return! Alt.always 8 }))

          testCase "do! Job.unit and Alt.unit"
          <| fun () ->
              let result =
                  job {
                      do! Job.unit ()
                      do! Alt.unit ()
                      return 1
                  }
                  |> run

              eq 1 result

          testCase "try/with catches raises"
          <| fun () ->
              let result =
                  job {
                      try
                          return! Job.raises (TestExn "boom")
                      with :? TestExn ->
                          return 42
                  }
                  |> run

              eq 42 result

          testCase "try/finally runs compensation"
          <| fun () ->
              let flag = ref false

              let result =
                  job {
                      try
                          return 1
                      finally
                          flag := true
                  }
                  |> run

              eq 1 result
              Expect.isTrue flag.Value "finally should run"

          testCase "use disposes resource"
          <| fun () ->
              let disposed = ref false

              let result =
                  job {
                      use _d =
                          { new IDisposable with
                              member _.Dispose() = disposed := true }

                      return 1
                  }
                  |> run

              eq 1 result
              Expect.isTrue disposed.Value "resource should be disposed"

          testCase "while loops"
          <| fun () ->
              let n = ref 0

              let result =
                  job {
                      while n.Value < 3 do
                          n := n.Value + 1

                      return n.Value
                  }
                  |> run

              eq 3 result

          testCase "for loops a sequence"
          <| fun () ->
              let acc = ref 0

              let result =
                  job {
                      for i in 1..3 do
                          acc := acc.Value + i

                      return acc.Value
                  }
                  |> run

              eq 6 result ]
