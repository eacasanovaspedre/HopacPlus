namespace HopacPlus

module HopacMailbox = Hopac.Mailbox

/// <summary>A buffered mailbox wrapped so Job.toHopac can resolve it.</summary>
[<Struct>]
type Mailbox<'T> =
    | Mailbox of HopacMailbox<'T>

    static member ToHopac(Mailbox x) : HopacMailbox<'T> = x

/// <summary>Operations on buffered mailboxes.</summary>
module Mailbox =
    let inline toHopac (x: ^a) : #HopacMailbox<'t> = (^a: (static member ToHopac: ^a -> #HopacMailbox<'t>) x)

    /// <summary>Creates a new buffered mailbox.</summary>
    let inline create () = Hopac.Mailbox () |> Mailbox

    /// <summary>
    /// Creates an alternative that becomes available when the mailbox contains at
    /// least one value and, if committed to, takes a value from the mailbox.
    /// </summary>
    let inline take (xMb: '``Mailbox<'x>``) = xMb |> toHopac |> HopacMailbox.take |> Alt

    /// <summary>
    /// Creates a job that sends the given value to the specified mailbox.  This
    /// operation never blocks.
    /// </summary>
    let inline send (xMb: '``Mailbox<'x>``) x = HopacMailbox.send (toHopac xMb) x |> Job

    /// <summary>Immediate or non-workflow operations on buffered mailboxes.</summary>
    module Now =
        /// <summary>
        /// Sends the given value to the specified mailbox.  <c>Mailbox.Now.send xMb
        /// x</c> is equivalent to <c>Mailbox.send xMb x |&gt; start</c>.
        /// </summary>
        let inline send (xMb: '``Mailbox<'x>``) x = HopacMailbox.Now.send (toHopac xMb) x
