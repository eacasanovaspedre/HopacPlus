namespace HopacPlus

module HopacLatch = Hopac.Latch

/// <summary>A latch wrapped so Job.toHopac can resolve it.</summary>
[<Struct>]
type Latch =
    | Latch of HopacLatch

    static member ToHopac(Latch x) : HopacLatch = x

/// <summary>Operations on latches.</summary>
module Latch =
    let inline toHopac (x: ^a) : HopacLatch = (^a: (static member ToHopac: ^a -> HopacLatch) x)

    /// <summary>Creates a new latch with the given initial count.</summary>
    let inline create n = Hopac.Latch n |> Latch

    /// <summary>
    /// Returns a job that explicitly decrements the counter of the latch.  When
    /// the counter reaches 0, the latch becomes open and operations awaiting
    /// the latch are resumed.
    /// </summary>
    let inline decrement (l: 'Latch) = l |> toHopac |> HopacLatch.decrement |> Job

    /// <summary>
    /// Creates a job that queues the given job to run as a separate concurrent
    /// job and holds the latch until the queued job either returns or fails with
    /// an exception.
    /// </summary>
    let inline queue (l: 'Latch) (uJ: '``Job<unit>``) = HopacLatch.queue (toHopac l) (Job.toHopac uJ) |> Job

    /// <summary>
    /// Creates a job that queues the given job to run as a separate concurrent
    /// job and holds the latch until the queued job either returns or fails with
    /// an exception.  A promise is returned for observing the result or failure
    /// of the queued job.
    /// </summary>
    let inline queueAsPromise (l: 'Latch) (xJ: '``Job<'x>``) =
        Job (Hopac.Job.map Promise (HopacLatch.queueAsPromise (toHopac l) (Job.toHopac xJ)))

    /// <summary>
    /// Creates a job that runs the given job holding the specified latch.
    /// </summary>
    let inline holding (l: 'Latch) (xJ: '``Job<'x>``) = HopacLatch.holding (toHopac l) (Job.toHopac xJ) |> Job

    /// <summary>
    /// Creates a job that creates a new latch, passes it to the given function to
    /// create a new job to run and then awaits for the latch to open.
    /// </summary>
    let inline within ([<InlineIfLambda>] l2xJ: Latch -> Job<'x>) =
        HopacLatch.within (fun l ->
            let (Job j) = l2xJ (Latch l)
            j)
        |> Job

    /// <summary>
    /// Returns an alternative that becomes available once the latch opens.
    /// </summary>
    let inline await (l: 'Latch) = l |> toHopac |> HopacLatch.await |> Alt

    /// <summary>Immediate operations on latches.</summary>
    module Now =
        /// <summary>Increments the counter of the latch.</summary>
        let inline increment (l: 'Latch) = l |> toHopac |> HopacLatch.Now.increment
