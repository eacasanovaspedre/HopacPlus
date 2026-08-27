namespace HopacPlus

module HopacPromise = Hopac.Promise

/// <summary>Operations on promises.</summary>
module Promise =
    let inline toHopac (x: ^a) : #HopacPromise<'t> = (^a: (static member ToHopac: ^a -> #HopacPromise<'t>) x)

    /// <summary>
    /// Creates an alternative for reading the promise.  If the promise was
    /// delayed, it is started as a separate job.
    /// </summary>
    let inline read (x: '``Promise<'x>``) = x |> toHopac |> HopacPromise.read |> Alt

    /// <summary>
    /// Creates a job that creates a promise, whose value is computed with the
    /// given job, which is immediately started to run as a separate concurrent
    /// job.  See also: queue, Job.queue.
    /// </summary>
    let inline start (x: '``Job<'x>``) = Job (Hopac.Job.map Promise (HopacPromise.start (Job.toHopac x)))

    /// <summary>
    /// Creates a job that creates a promise, whose value is computed with the
    /// given job, which is scheduled to be run as a separate concurrent job.  See
    /// also: start, Job.queue.
    /// </summary>
    let inline queue (x: '``Job<'x>``) = Job (Hopac.Job.map Promise (HopacPromise.queue (Job.toHopac x)))

    /// <summary>Immediate or non-workflow operations on promises.</summary>
    module Now =
        /// <summary>
        /// Returns the value or raises the failure exception that the promise has
        /// been fulfilled with.  It is considered an error if the promise has not
        /// yet been fulfilled.
        /// </summary>
        let inline get (x: '``Promise<'x>``) = x |> toHopac |> HopacPromise.Now.get

        /// <summary>
        /// Returns true iff the given promise has already been fulfilled (either
        /// with a value or with a failure).
        /// </summary>
        let inline isFulfilled (x: '``Promise<'x>``) = x |> toHopac |> HopacPromise.Now.isFulfilled

        /// <summary>Creates a promise that will never be fulfilled.</summary>
        let inline never () = HopacPromise.Now.never () |> Promise

        /// <summary>Creates a promise with the given failure exception.</summary>
        let inline withFailure e = HopacPromise.Now.withFailure e |> Promise

        /// <summary>Creates a promise with the given value.</summary>
        let inline withValue x = HopacPromise.Now.withValue x |> Promise

        /// <summary>
        /// Creates a promise whose value is computed lazily with the given job when
        /// an attempt is made to read the promise.
        /// </summary>
        let inline delay (x: '``Job<'x>``) = x |> Job.toHopac |> HopacPromise.Now.delay |> Promise
