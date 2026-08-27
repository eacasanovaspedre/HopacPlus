module HopacPlus.Tests.JobTests

open System
open System.Threading.Tasks
open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "Job"
        [ testCase "result and unit"
          <| fun () ->
              eq 1 (run (Job.result 1))
              eq () (run (Job.unit ()))

          testCase "thunk and lift"
          <| fun () ->
              eq 4 (run (Job.thunk (fun () -> 4)))
              eq 6 (run (Job.lift ((+) 1) 5))

          testCase "raises" <| fun () -> throws (Job.raises (TestExn "x"))

          testCase "map bind delay join apply Ignore"
          <| fun () ->
              eq 3 (run (Job.map ((+) 1) (Job.result 2)))
              eq 3 (run (Job.bind (fun x -> Job.result (x + 1)) (Job.result 2)))

              eq
                  3
                  (run (
                      job {
                          let! x = Alt.always 2
                          return x + 1
                      }
                  ))

              eq 9 (run (Job.delay (fun () -> Job.result 9)))
              eq 8 (run (Job.delayWith (fun x -> Job.result (x * 2)) 4))
              eq 1 (run (Job.join (Job.result (Job.result 1))))
              eq 3 (run (Job.apply (Job.result 1) (Job.result ((+) 2))))
              eq () (run (Job.Ignore(Job.result 1)))

          testCase "tryIn tryWith tryFinally catch"
          <| fun () ->
              eq 2 (run (Job.tryIn (Job.result 1) (fun x -> Job.result (x + 1)) (fun _ -> Job.result -1)))
              eq 7 (run (Job.tryIn (Job.raises (TestExn "e")) (fun x -> Job.result x) (fun _ -> Job.result 7)))

              eq
                  3
                  (run (Job.tryInDelay (fun () -> Job.result 1) (fun x -> Job.result (x + 2)) (fun _ -> Job.result -1)))

              eq 4 (run (Job.tryWith (Job.raises (TestExn "e")) (fun _ -> Job.result 4)))
              eq 5 (run (Job.tryWithDelay (fun () -> Job.raises (TestExn "e")) (fun _ -> Job.result 5)))

              let flag = ref false
              eq 1 (run (Job.tryFinallyFun (Job.result 1) (fun () -> flag := true)))
              Expect.isTrue flag.Value ""

              flag := false
              eq 1 (run (Job.tryFinallyFunDelay (fun () -> Job.result 1) (fun () -> flag := true)))
              Expect.isTrue flag.Value ""

              flag := false
              eq 1 (run (Job.tryFinallyJob (Job.result 1) (Job.thunk (fun () -> flag := true))))
              Expect.isTrue flag.Value ""

              flag := false
              eq 1 (run (Job.tryFinallyJobDelay (fun () -> Job.result 1) (Job.thunk (fun () -> flag := true))))
              Expect.isTrue flag.Value ""

              match run (Job.catch (Job.result 1)) with
              | Choice1Of2 1 -> ()
              | other -> failtestf "unexpected %A" other

              match run (Job.catch (Job.raises (TestExn "e"))) with
              | Choice2Of2(:? TestExn) -> ()
              | other -> failtestf "unexpected %A" other

          testCase "using useIn"
          <| fun () ->
              let disposed = ref false

              let resource =
                  { new IDisposable with
                      member _.Dispose() = disposed := true }

              eq 1 (run (Job.using resource (fun _ -> Job.result 1)))
              Expect.isTrue disposed.Value ""

              disposed := false
              eq 1 (run (Job.useIn (fun _ -> Job.result 1) resource))
              Expect.isTrue disposed.Value ""

          testCase "loops"
          <| fun () ->
              let n = ref 0
              run (Job.forN 3 (Job.thunk (fun () -> n := n.Value + 1)))
              eq 3 n.Value

              n := 0
              run (Job.forNIgnore 2 (Job.result 1))
              n := 0
              run (Job.forUpTo 1 3 (fun i -> Job.thunk (fun () -> n := n.Value + i)))
              eq 6 n.Value

              n := 0
              run (Job.forUpToIgnore 1 2 (fun i -> Job.result i))
              n := 0
              run (Job.forDownTo 3 1 (fun i -> Job.thunk (fun () -> n := n.Value + i)))
              eq 6 n.Value

              n := 0
              eq () (run (Job.whileDo (fun () -> n.Value < 3) (Job.thunk (fun () -> n := n.Value + 1))))
              eq 3 n.Value

              n := 0
              run (Job.whileDoDelay (fun () -> n.Value < 2) (fun () -> Job.thunk (fun () -> n := n.Value + 1)))
              eq 2 n.Value

              n := 0
              run (Job.whileDoIgnore (fun () -> n.Value < 1) (Job.thunk (fun () -> n := n.Value + 1)))
              eq 1 n.Value

              eq () (run (Job.whenDo true (Job.unit ())))
              eq () (run (Job.whenDo false (Job.raises (TestExn "no"))))

          testCase "seq and con collect"
          <| fun () ->
              let xs = run (Job.seqCollect [ Job.result 1; Job.result 2; Job.result 3 ])
              eq [ 1; 2; 3 ] (List.ofSeq xs)

              let ys = run (Job.conCollect [ Job.result 4; Job.result 5 ])
              Expect.containsAll (List.ofSeq ys) [ 4; 5 ] ""

              run (Job.seqIgnore [ Job.result 1; Job.result 2 ])
              run (Job.conIgnore [ Job.result 1; Job.result 2 ])

          testCase "async and task"
          <| fun () ->
              eq 3 (run (Job.fromAsync (async { return 3 })))
              eq 3 (Async.RunSynchronously(Job.toAsync (Job.result 3)))
              eq 4 (run (Job.bindAsync (fun x -> Job.result (x + 1)) (async { return 3 })))
              eq 5 (run (Job.fromTask (fun () -> Task.FromResult 5)))
              eq () (run (Job.fromUnitTask (fun () -> Task.CompletedTask)))
              eq 6 (run (Job.liftTask (fun x -> Task.FromResult(x + 1)) 5))
              eq () (run (Job.liftUnitTask (fun _ -> Task.CompletedTask) 1))
              eq 7 (run (Job.awaitTask (Task.FromResult 7)))
              eq () (run (Job.awaitUnitTask Task.CompletedTask))
              eq 8 (run (Job.bindTask (fun x -> Job.result (x + 1)) (Task.FromResult 7)))
              eq 9 (run (Job.bindUnitTask (fun () -> Job.result 9) Task.CompletedTask))

          testCase "start and queue fill an IVar"
          <| fun () ->
              let iv = Hopac.IVar()

              eq
                  1
                  (run (
                      job {
                          do! Job.start (iv *<= 1)
                          return! iv
                      }
                  ))

              let iv2 = Hopac.IVar()

              eq
                  2
                  (run (
                      job {
                          do! Job.queue (iv2 *<= 2)
                          return! iv2
                      }
                  ))

          testCase "abort in a started job does not kill the parent"
          <| fun () ->
              let iv = Hopac.IVar()

              eq
                  1
                  (run (
                      job {
                          do!
                              Job.start (
                                  job {
                                      do! Job.abort ()
                                      do! iv *<= 99
                                  }
                              )

                          do! timeOutMillis 20
                          return 1
                      }
                  ))

          testCase "paranoid preserves the result"
          <| fun () -> eq 1 (run (Job.paranoid (Job.result 1)))

          testCase "Random.get returns a value"
          <| fun () ->
              let n = run (Job.Random.get ())
              ignore n
              eq 3 (run (Job.Random.map (fun _ -> 3)))
              eq 4 (run (Job.Random.bind (fun _ -> Job.result 4)))

          testCase "Scheduler.get and switchToWorker"
          <| fun () ->
              let _s = run (Job.Scheduler.get ())

              eq
                  1
                  (run (
                      job {
                          do! Job.Scheduler.switchToWorker ()
                          return 1
                      }
                  ))

              eq 2 (run (Job.Scheduler.isolate (fun () -> 2)))

          testCase "Job.Global.run" <| fun () -> eq 1 (Job.Global.run (Job.result 1))

          testCase "top-level run start queue memo"
          <| fun () ->
              eq 1 (run (Job.result 1))
              eq 2 (runDelay (fun () -> Job.result 2))

              let iv = Hopac.IVar()
              start (iv *<= 3)
              eq 3 (runNative iv)

              let iv2 = Hopac.IVar()
              queue (iv2 *<= 4)
              eq 4 (runNative iv2)

              eq 5 (runNative (memo (Job.result 5)))

          testProperty "map id" <| fun (x: int) -> run (Job.map id (Job.result x)) = x

          testProperty "map composition"
          <| fun (x: int) ->
              let f i = i + 1
              let g i = i * 2
              run (Job.map (f >> g) (Job.result x)) = run (Job.map g (Job.map f (Job.result x)))

          testProperty "left identity"
          <| fun (x: int) -> run (Job.map (fun y -> y + 1) (Job.result x)) = x + 1

          testProperty "right identity"
          <| fun (x: int) -> run (Job.bind Job.result (Job.result x)) = x

          testProperty "associativity"
          <| fun (x: int) ->
              let f y = Job.result (y + 1)
              let g y = Job.result (y * 2)
              run (Job.bind g (Job.bind f (Job.result x))) = run (Job.bind (fun y -> Job.bind g (f y)) (Job.result x)) ]
