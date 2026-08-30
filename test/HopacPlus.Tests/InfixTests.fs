module HopacPlus.Tests.InfixTests

open System
open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "Infixes"
        [ testCase "sequencing"
          <| fun () ->
              eq 2 (run (Job.result 1 >>= fun x -> Job.result (x + 1)))
              eq 2 (run (Alt.always 1 >>= fun x -> Job.result (x + 1)))
              eq 2 (run (Job.result 1 >>= fun x -> Alt.always (x + 1)))

              eq
                  2
                  (run (
                      job {
                          let! x = Alt.always 1
                          return x + 1
                      }
                  ))

              eq 2 (run (Job.result 1 >>- ((+) 1)))
              eq 2 (run (Alt.always 1 >>- ((+) 1)))
              eq 2 (run (Job.result () >>=. Job.result 2))
              eq 2 (run (Alt.always () >>=. Job.result 2))
              eq 2 (run (Job.result () >>=. Alt.always 2))
              eq 2 (run (Job.result "x" >>-. 2))
              eq 2 (run (Alt.always "x" >>-. 2))
              throws (Job.result () >>-! TestExn "e")
              throws (Alt.always () >>-! TestExn "e")

          testCase "memo sequencing"
          <| fun () ->
              eq 2 (runNative (Job.result 1 >>=* fun x -> Job.result (x + 1)))
              eq 2 (runNative (Job.result 1 >>-* ((+) 1)))
              eq 2 (runNative (Job.result () >>=*. Job.result 2))
              eq 2 (runNative (Job.result "x" >>-*. 2))
              throws (job { return! Job.result () >>-*! TestExn "e" })

          testCase "composition"
          <| fun () ->
              eq 2 (run ((Job.result >=> fun x -> Job.result (x + 1)) 1))
              eq 2 (run ((Alt.always >=> fun x -> Job.result (x + 1)) 1))
              eq 2 (run ((Job.result >=> fun x -> Alt.always (x + 1)) 1))
              eq 2 (run ((Job.result >-> ((+) 1)) 1))
              eq 2 (run ((Alt.always >-> ((+) 1)) 1))
              eq 2 (run ((Job.result >=>. Job.result 2) ()))
              eq 2 (run ((Alt.always >=>. Job.result 2) ()))
              eq 2 (run ((Job.result >=>. Alt.always 2) ()))
              eq 2 (run ((Job.result >->. 2) ()))
              eq 2 (run ((Alt.always >->. 2) ()))
              throws ((Job.result >->! TestExn "e") ())
              throws ((Alt.always >->! TestExn "e") ())

          testCase "memo composition"
          <| fun () ->
              eq 2 (runNative ((Job.result >=>* fun x -> Job.result (x + 1)) 1))
              eq 2 (runNative ((Job.result >->* ((+) 1)) 1))
              eq 2 (runNative ((Job.result >=>*. Job.result 2) ()))
              eq 2 (runNative ((Job.result >->*. 2) ()))
              throws (job { return! (Job.result >->*! TestExn "e") () })

          testCase "pairing"
          <| fun () ->
              eq (1, 2) (run (Job.result 1 <&> Job.result 2))
              eq (1, 2) (run (Alt.always 1 <&> Job.result 2))
              eq (1, 2) (run (Job.result 1 <&> Alt.always 2))
              eq (1, 2) (run (Job.result 1 <*> Job.result 2))
              eq (1, 2) (run (Alt.always 1 <*> Job.result 2))
              eq (1, 2) (run (Job.result 1 <*> Alt.always 2))
              eq (1, 2) (run (Alt.always 1 <+> Alt.always 2))

          testCase "alt after"
          <| fun () ->
              eq 2 (run (Alt.always 1 ^=> fun x -> Job.result (x + 1)))
              eq 2 (run (Alt.always 1 ^-> ((+) 1)))
              eq 2 (run (Alt.always () ^=>. Job.result 2))
              eq 2 (run (Alt.always () ^->. 2))
              throws (job { return! Alt.always () ^->! TestExn "e" })

          testCase "choice"
          <| fun () ->
              eq 1 (run (Alt.always 1 <|> Alt.never ()))
              eq 1 (runNative (Alt.always 1 <|>* Alt.never ()))
              let n = run (Alt.always 1 <~> Alt.always 1)
              eq 1 n
              eq 1 (runNative (Alt.always 1 <~>* Alt.always 1))

          testCase "message passing"
          <| fun () ->
              let ch = Ch.create ()

              eq
                  1
                  (run (
                      job {
                          do! Job.start (ch *<+ 1)
                          return! ch
                      }
                  ))

              let ch2 = Ch.create ()

              eq
                  2
                  (run (
                      job {
                          do! Job.start (job { return! ch2 *<- 2 })
                          return! ch2
                      }
                  ))

              let iv = IVar.create ()

              eq
                  3
                  (run (
                      job {
                          do! iv *<= 3
                          return! iv
                      }
                  ))

              let ivf = IVar.create ()

              throws (
                  job {
                      do! ivf *<=! TestExn "e"
                      return! ivf
                  }
              )

              let mv = MVar.create ()

              eq
                  4
                  (run (
                      job {
                          do! mv *<<= 4
                          return! mv
                      }
                  ))

              let mb = Mailbox.create ()

              eq
                  5
                  (run (
                      job {
                          do! mb *<<+ 5
                          return! mb
                      }
                  ))

          testCase "query *<-=>="
          <| fun () ->
              eq
                  2
                  (run (
                      job {
                          let q = Ch.create ()

                          do!
                              Job.start (
                                  job {
                                      let! n, rI = q
                                      do! (rI *<= (n + 1))
                                  }
                              )

                          return! q *<-=>= fun rI -> Job.result (1, rI)
                      }
                  )) ]
