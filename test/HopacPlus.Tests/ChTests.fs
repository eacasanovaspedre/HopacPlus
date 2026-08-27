module HopacPlus.Tests.ChTests

open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "Ch"
        [ testCase "give and take"
          <| fun () ->
              let ch = Ch.create ()

              eq
                  1
                  (run (
                      job {
                          do! Job.start (job { return! Ch.give ch 1 })
                          return! Ch.take ch
                      }
                  ))

          testCase "send"
          <| fun () ->
              let ch = Ch.create ()

              eq
                  2
                  (run (
                      job {
                          do! Ch.send ch 2
                          return! Ch.take ch
                      }
                  ))

          testCase "let! on channel"
          <| fun () ->
              let ch = Ch.create ()

              eq
                  3
                  (run (
                      job {
                          do! ch *<+ 3
                          return! ch
                      }
                  ))

          testCase "Try.give and Try.take"
          <| fun () ->
              let ch = Ch.create ()
              eq None (run (Ch.Try.take ch))
              eq false (run (Ch.Try.give ch 1))

          testCase "Now.send"
          <| fun () ->
              let ch = Ch.create ()
              Ch.Now.send ch 4
              eq 4 (run (Ch.take ch)) ]
