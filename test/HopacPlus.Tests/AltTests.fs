module HopacPlus.Tests.AltTests

open System
open System.Threading
open System.Threading.Tasks
open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "Alt"
        [ testCase "always unit once return the correct value"
          <| fun () ->
              eq 1 (run (Alt.always 1))
              eq () (run (Alt.unit ()))
              eq 2 (run (Alt.once 2))
              eq 2 (run (Alt.once 2))

          testCase "never loses to always"
          <| fun () ->
              eq 1 (run (Alt.choose [ Alt.never (); Alt.always 1 ]))
              eq () (run (Alt.choose [ Alt.zero (); Alt.unit () ]))

          testCase "raises" <| fun () -> throws (job { return! Alt.raises (TestExn "e") })

          testCase "Ignore afterFun afterJob"
          <| fun () ->
              eq () (run (Alt.Ignore(Alt.always 1)))
              eq 3 (run (Alt.afterFun ((+) 1) (Alt.always 2)))
              eq 3 (run (Alt.afterJob (fun x -> Job.result (x + 1)) (Alt.always 2)))

          testCase "choose choosy chooser"
          <| fun () ->
              eq 2 (run (Alt.choose [ Alt.always 2; Alt.never () ]))
              eq 3 (run (Alt.choosy [| Alt.never (); Alt.always 3 |]))
              let n = run (Alt.chooser [ Alt.always 1; Alt.always 1 ])
              eq 1 n

          testCase "prepareFun prepare prepareJob"
          <| fun () ->
              eq 4 (run (Alt.prepareFun (fun () -> Alt.always 4)))
              eq 5 (run (Alt.prepare (Job.result (Alt.always 5))))
              eq 6 (run (Alt.prepareJob (fun () -> Job.result (Alt.always 6))))

          testCase "random" <| fun () -> eq 7 (run (Alt.random (fun _ -> Alt.always 7)))

          testCase "withNackFun still chooses the ready branch"
          <| fun () ->
              let loser = Alt.withNackFun (fun _ -> timeOutMillis 500)
              eq 1 (run (Alt.choose [ Alt.always 1; loser ^->. 0 ]))

          testCase "wrapAbortFun still chooses the ready branch"
          <| fun () ->
              let loser = Alt.wrapAbortFun ignore (timeOutMillis 500)
              eq 1 (run (Alt.choose [ Alt.always 1; loser ^->. 0 ]))

          testCase "wrapAbortJob still chooses the ready branch"
          <| fun () ->
              let loser = Alt.wrapAbortJob (Job.unit ()) (timeOutMillis 500)
              eq 1 (run (Alt.choose [ Alt.always 1; loser ^->. 0 ]))

          testCase "tryIn tryFinallyFun tryFinallyJob"
          <| fun () ->
              eq 2 (run (Alt.tryIn (Alt.always 1) (fun x -> Job.result (x + 1)) (fun _ -> Job.result -1)))
              eq 9 (run (Alt.tryIn (Alt.raises (TestExn "e")) (fun x -> Job.result x) (fun _ -> Job.result 9)))

              let flag = ref false
              eq 1 (run (Alt.tryFinallyFun (Alt.always 1) (fun () -> flag := true)))
              Expect.isTrue flag.Value ""

              flag := false
              eq 1 (run (Alt.tryFinallyJob (Alt.always 1) (Job.thunk (fun () -> flag := true))))
              Expect.isTrue flag.Value ""

          testCase "fromAsync fromTask fromUnitTask"
          <| fun () ->
              eq 8 (run (Alt.fromAsync (async { return 8 })))
              eq 9 (run (Alt.fromTask (fun (_: CancellationToken) -> Task.FromResult 9)))
              eq () (run (Alt.fromUnitTask (fun (_: CancellationToken) -> Task.CompletedTask)))

          testCase "toAsync"
          <| fun () -> eq 1 (Async.RunSynchronously(Alt.toAsync (Alt.always 1)))

          testCase "timeOut loses to always"
          <| fun () ->
              eq 1 (run (Alt.choose [ timeOut (TimeSpan.FromMilliseconds 500.) ^->. 0; Alt.always 1 ]))
              eq 1 (run (Alt.choose [ timeOutMillis 500 ^->. 0; Alt.always 1 ]))

          testCase "idle becomes available" <| fun () -> eq () (run Alt.idle)

          testCase "paranoid preserves the result"
          <| fun () -> eq 1 (run (Alt.paranoid (Alt.always 1)))

          testCase "choice prefers the first ready alternative"
          <| fun () ->
              eq 1 (run (Alt.always 1 <|> Alt.always 2))
              eq 1 (run (Alt.always 1 <|> Alt.never ()))

          testCase "<+> commits both in sequence"
          <| fun () -> eq (1, 2) (run (Alt.always 1 <+> Alt.always 2)) ]
