namespace HopacPlus

module HopacIVar = Hopac.IVar

/// <summary>A write-once variable wrapped so Job.toHopac can resolve it.</summary>
[<Struct>]
type IVar<'T> =
    | IVar of HopacIVar<'T>

    static member ToHopac(IVar x) : HopacIVar<'T> = x

/// <summary>Operations on write once variables.</summary>
module IVar =
    let inline toHopac (x: ^a) : #HopacIVar<'t> = (^a: (static member ToHopac: ^a -> #HopacIVar<'t>) x)

    /// <summary>Creates a new write once variable.</summary>
    let inline create () = Hopac.IVar () |> IVar

    /// <summary>Creates a new write once variable with the given value.</summary>
    let createFull (x: 'x) : IVar<'x> = IVar (new Hopac.IVar<'x> (x))

    /// <summary>Creates a new write once variable with the given failure exception.</summary>
    let createFailure (e: exn) : IVar<'x> = IVar (new Hopac.IVar<'x> (e))

    /// <summary>
    /// Creates an alternative that becomes available after the write once
    /// variable has been written to.
    /// </summary>
    let inline read (xI: '``IVar<'x>``) = xI |> toHopac |> HopacIVar.read |> Alt

    /// <summary>
    /// Creates a job that writes the given value to the given write once
    /// variable.  It is an error to write to a single write once variable more
    /// than once.
    /// </summary>
    let inline fill (xI: '``IVar<'x>``) x = HopacIVar.fill (toHopac xI) x |> Job

    /// <summary>
    /// Creates a job that tries to write the given value to the given write once
    /// variable.  No operation takes place and no error is reported in case the
    /// write once variable has already been written to.
    /// </summary>
    let inline tryFill (xI: '``IVar<'x>``) x = HopacIVar.tryFill (toHopac xI) x |> Job

    /// <summary>
    /// Creates a job that writes the given exception to the given write once
    /// variable.  It is an error to write to a single IVar more than once.
    /// </summary>
    let inline fillFailure (xI: '``IVar<'x>``) e = HopacIVar.fillFailure (toHopac xI) e |> Job

    /// <summary>
    /// Creates a job that tries to write the given exception to the given write
    /// once variable.  No operation takes place and no error is reported in case
    /// the write once variable has already been written to.
    /// </summary>
    let inline tryFillFailure (xI: '``IVar<'x>``) e = HopacIVar.tryFillFailure (toHopac xI) e |> Job

    /// <summary>Immediate or non-workflow operations on write once variables.</summary>
    module Now =
        /// <summary>
        /// Returns the value or raises the failure exception written to the write
        /// once variable.  It is considered an error if the write once variable has
        /// not yet been written to.
        /// </summary>
        let inline get (xI: '``IVar<'x>``) = xI |> toHopac |> HopacIVar.Now.get

        /// <summary>
        /// Returns true iff the given write once variable has already been filled
        /// (either with a value or with a failure).
        /// </summary>
        let inline isFull (xI: '``IVar<'x>``) = xI |> toHopac |> HopacIVar.Now.isFull
