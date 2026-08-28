namespace HopacPlus

module HopacBoundedMb = Hopac.BoundedMb

/// <summary>A bounded synchronous mailbox wrapped so Job.toHopac can resolve it.</summary>
[<Struct>]
type BoundedMb<'T> =
    | BoundedMb of HopacBoundedMb<'T>

    static member inline ToHopac(BoundedMb x) : HopacBoundedMb<'T> = x

/// <summary>Operations on bounded synchronous mailboxes.</summary>
module BoundedMb =
    let inline toHopac (x: ^a) : #HopacBoundedMb<'t> = (^a: (static member ToHopac: ^a -> #HopacBoundedMb<'t>) x)

    /// <summary>Creates a new bounded mailbox with the given capacity.</summary>
    let inline create n = Hopac.BoundedMb n |> BoundedMb

    /// <summary>
    /// Selective synchronous operation to take a message from a bounded mailbox.
    /// </summary>
    let inline take (xB: '``BoundedMb<'x>``) = xB |> toHopac |> HopacBoundedMb.take |> Alt

    /// <summary>
    /// Selective synchronous operation to put a message to a bounded mailbox.
    /// </summary>
    let inline put (xB: '``BoundedMb<'x>``) x = HopacBoundedMb.put (toHopac xB) x |> Alt
