namespace HopacPlus

module HopacMVar = Hopac.MVar

/// <summary>A serialized variable wrapped so Job.toHopac can resolve it.</summary>
[<Struct>]
type MVar<'T> =
    | MVar of HopacMVar<'T>

    static member inline ToHopac(MVar x) : HopacMVar<'T> = x

/// <summary>Operations on serialized variables.</summary>
module MVar =
    let inline toHopac (x: ^a) : #HopacMVar<'t> = (^a: (static member ToHopac: ^a -> #HopacMVar<'t>) x)

    /// <summary>Creates a new empty serialized variable.</summary>
    let inline create () = Hopac.MVar () |> MVar

    /// <summary>Creates a new serialized variable filled with the given value.</summary>
    let inline createFull x = Hopac.MVar x |> MVar

    /// <summary>
    /// Creates a job that writes the given value to the serialized variable.  It
    /// is an error to write to a MVar that is full.
    /// </summary>
    let inline fill (xM: '``MVar<'x>``) x = HopacMVar.fill (toHopac xM) x |> Job

    /// <summary>
    /// Creates an alternative that becomes available when the variable contains a
    /// value and, if committed to, takes the value from the variable.
    /// </summary>
    let inline take (xM: '``MVar<'x>``) = xM |> toHopac |> HopacMVar.take |> Alt

    /// <summary>
    /// Creates an alternative that becomes available when the variable contains a
    /// value and, if committed to, read the value from the variable.
    /// </summary>
    let inline read (xM: '``MVar<'x>``) = xM |> toHopac |> HopacMVar.read |> Alt

    /// <summary>
    /// Creates an alternative that takes the value of the serialized variable and
    /// then fills the variable with the result of performing the given function.
    /// </summary>
    let inline modifyFun ([<InlineIfLambda>] x2xy) (xM: '``MVar<'x>``) = HopacMVar.modifyFun x2xy (toHopac xM) |> Alt

    /// <summary>
    /// Creates an alternative that takes the value of the serialized variable and
    /// then fills the variable with the result of performing the given job.
    /// </summary>
    let inline modifyJob ([<InlineIfLambda>] x2xyJ) (xM: '``MVar<'x>``) =
        HopacMVar.modifyJob (Job.toHopacF x2xyJ) (toHopac xM) |> Alt

    /// <summary>
    /// Like modifyFun except that if the function raises, the variable is filled
    /// with its original value before propagating the exception.
    /// </summary>
    let inline tryModifyFun ([<InlineIfLambda>] x2xy) (xM: '``MVar<'x>``) =
        HopacMVar.tryModifyFun x2xy (toHopac xM) |> Alt

    /// <summary>
    /// Like modifyJob except that if the job raises, the variable is filled
    /// with its original value before propagating the exception.
    /// </summary>
    let inline tryModifyJob ([<InlineIfLambda>] x2xyJ) (xM: '``MVar<'x>``) =
        HopacMVar.tryModifyJob (Job.toHopacF x2xyJ) (toHopac xM) |> Alt

    /// <summary>
    /// Creates an alternative that takes the value of the serialized variable and
    /// then fills the variable with the result of performing the given function.
    /// </summary>
    let inline mutateFun ([<InlineIfLambda>] x2x) (xM: '``MVar<'x>``) = HopacMVar.mutateFun x2x (toHopac xM) |> Alt

    /// <summary>
    /// Creates an alternative that takes the value of the serialized variable and
    /// then fills the variable with the result of performing the given job.
    /// </summary>
    let inline mutateJob ([<InlineIfLambda>] x2xJ) (xM: '``MVar<'x>``) =
        HopacMVar.mutateJob (Job.toHopacF x2xJ) (toHopac xM) |> Alt

    /// <summary>
    /// Like mutateFun except that if the function raises, the variable is filled
    /// with its original value before propagating the exception.
    /// </summary>
    let inline tryMutateFun ([<InlineIfLambda>] x2x) (xM: '``MVar<'x>``) =
        HopacMVar.tryMutateFun x2x (toHopac xM) |> Alt

    /// <summary>
    /// Like mutateJob except that if the job raises, the variable is filled
    /// with its original value before propagating the exception.
    /// </summary>
    let inline tryMutateJob ([<InlineIfLambda>] x2xJ) (xM: '``MVar<'x>``) =
        HopacMVar.tryMutateJob (Job.toHopacF x2xJ) (toHopac xM) |> Alt
