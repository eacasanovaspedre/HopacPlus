namespace HopacPlus

module HopacLock = Hopac.Lock

/// <summary>A mutual exclusion lock wrapped so Job.toHopac can resolve it.</summary>
[<Struct>]
type Lock =
    | Lock of HopacLock

    static member inline ToHopac(Lock x) : HopacLock = x

/// <summary>Operations on mutual exclusion locks.</summary>
module Lock =
    let inline toHopac (x: ^a) : HopacLock = (^a: (static member ToHopac: ^a -> HopacLock) x)

    /// <summary>Creates a new mutual exclusion lock.</summary>
    let inline create () = Hopac.Lock () |> Lock

    /// <summary>
    /// Creates a job that calls the given function so that the lock is held
    /// during the execution of the function.
    /// </summary>
    let inline duringFun (l: 'Lock) ([<InlineIfLambda>] u2x) = HopacLock.duringFun (toHopac l) u2x |> Job

    /// <summary>
    /// Creates a job that runs the given job so that the lock is held during the
    /// execution of the given job.
    /// </summary>
    let inline duringJob (l: 'Lock) (xJ: '``Job<'x>``) = HopacLock.duringJob (toHopac l) (Job.toHopac xJ) |> Job
