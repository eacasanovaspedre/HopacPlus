module HopacPlus.Tests.MailboxTests

open Expecto
open HopacPlus
open HopacPlus.Infixes
open Helpers

[<Tests>]
let tests =
    testList
        "Mailbox"
        [ testCase "send and take"
          <| fun () ->
              let mb = Mailbox.create ()

              eq
                  1
                  (run (
                      job {
                          do! Mailbox.send mb 1
                          return! Mailbox.take mb
                      }
                  ))

          testCase "infix send"
          <| fun () ->
              let mb = Mailbox.create ()

              eq
                  2
                  (run (
                      job {
                          do! mb *<<+ 2
                          return! mb
                      }
                  )) ]
