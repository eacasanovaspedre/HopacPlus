namespace HopacPlus

module HopacCh = Hopac.Ch

/// <summary>A synchronous channel wrapped so FSharpPlus and Job.toHopac can resolve it.</summary>
[<Struct>]
type Ch<'T> =
    | Ch of HopacCh<'T>

    static member ToHopac(Ch c) : HopacCh<'T> = c

/// <summary>Operations on synchronous channels.</summary>
module Ch =
    let inline toHopac (x: ^a) : #HopacCh<'t> = (^a: (static member ToHopac: ^a -> #HopacCh<'t>) x)

    /// <summary>Creates a new synchronous channel.</summary>
    let inline create () = Hopac.Ch () |> Ch

    /// <summary>
    /// Creates an alternative that, at instantiation time, offers to give the
    /// given value on the given channel, and becomes available when another job
    /// offers to take the value.
    /// </summary>
    let inline give (xCh: '``Ch<'x>``) x = HopacCh.give (toHopac xCh) x |> Alt

    /// <summary>
    /// Creates a job that sends a value to another job on the given channel.  A
    /// send operation is asynchronous.
    /// </summary>
    let inline send (xCh: '``Ch<'x>``) x = HopacCh.send (toHopac xCh) x |> Job

    /// <summary>
    /// Creates an alternative that, at instantiation time, offers to take a
    /// value from another job on the given channel, and becomes available when
    /// another job offers to give a value.
    /// </summary>
    let inline take (xCh: '``Ch<'x>``) = xCh |> toHopac |> HopacCh.take |> Alt

    /// <summary>Immediate or non-workflow operations on synchronous channels.</summary>
    module Now =
        /// <summary>
        /// Sends the given value to the specified channel.  <c>Ch.Now.send xCh x</c>
        /// is equivalent to <c>Ch.send xCh x |&gt; start</c>.
        /// </summary>
        let inline send (xCh: '``Ch<'x>``) x = HopacCh.Now.send (toHopac xCh) x

    /// <summary>Selective operations that do not wait.</summary>
    module Try =
        /// <summary>
        /// Attempts to take a value from the given channel.  Returns None if no
        /// value is immediately available.
        /// </summary>
        let inline take (xCh: '``Ch<'x>``) = xCh |> toHopac |> HopacCh.Try.take |> Job

        /// <summary>
        /// Attempts to give a value on the given channel.  Returns false if no
        /// taker is immediately available.
        /// </summary>
        let inline give (xCh: '``Ch<'x>``) x = HopacCh.Try.give (toHopac xCh) x |> Job
