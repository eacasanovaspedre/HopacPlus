namespace HopacPlus

module NativeHopac = Hopac.Hopac

[<AutoOpen>]
module Hopac =

    let job = JobBuilder()

    /// <summary>
    /// Starts running the given job and then blocks the current thread waiting
    /// for the job to either return successfully or fail.  See also: start.
    /// </summary>
    let inline run x = NativeHopac.run (Job.toHopac x)

    /// <summary>
    /// <c>runDelay u2xJ</c> is equivalent to <c>run &lt;| Job.delay u2xJ</c>.
    /// </summary>
    let inline runDelay ([<InlineIfLambda>] u2xJ) =
        NativeHopac.runDelay (Job.toHopacF u2xJ)

    /// <summary>
    /// Starts running the given job, but does not wait for the job to finish.
    /// See also: queue, server.
    /// </summary>
    let inline start uJ = NativeHopac.start (Job.toHopac uJ)

    /// <summary>
    /// Starts running the given job, but does not wait for the job to finish.
    /// <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; start</c>.
    /// </summary>
    let inline startIgnore xJ =
        NativeHopac.startIgnore (Job.toHopac xJ)

    /// <summary>
    /// Starts running the given delayed job, but does not wait for the job to
    /// finish.  <c>startDelay u2xJ</c> is equivalent to <c>startIgnore &lt;| Job.delay
    /// u2xJ</c>.
    /// </summary>
    let inline startDelay ([<InlineIfLambda>] u2xJ) =
        NativeHopac.startDelay (Job.toHopacF u2xJ)

    /// <summary>
    /// Starts running the given job, but does not wait for the job to finish.
    /// Upon the failure or success of the job, one of the given actions is called
    /// once.
    /// </summary>
    let inline startWithActions e2u x2u x =
        NativeHopac.startWithActions e2u x2u (Job.toHopac x)

    /// <summary>
    /// Starts running the given job.  The result can be obtained from the
    /// returned task.
    /// </summary>
    let inline startAsTask x = NativeHopac.startAsTask (Job.toHopac x)

    /// <summary>
    /// Queues the given job for execution.  See also: start, server.
    /// </summary>
    let inline queue uJ = NativeHopac.queue (Job.toHopac uJ)

    /// <summary>
    /// Queues the given job for execution.  <c>queueIgnore xJ</c> is equivalent to
    /// <c>Job.Ignore xJ |&gt; queue</c>.
    /// </summary>
    let inline queueIgnore xJ =
        NativeHopac.queueIgnore (Job.toHopac xJ)

    /// <summary>
    /// Queues the given delayed job for execution.  <c>queueDelay u2xJ</c> is
    /// equivalent to <c>queueIgnore &lt;| Job.delay u2xJ</c>.
    /// </summary>
    let inline queueDelay ([<InlineIfLambda>] u2xJ) =
        NativeHopac.queueDelay (Job.toHopacF u2xJ)

    /// <summary>
    /// Queues the given job for execution.  The result can be obtained from the
    /// returned task.
    /// </summary>
    let inline queueAsTask x = NativeHopac.queueAsTask (Job.toHopac x)

    /// <summary>
    /// Starts running the given job like start, but the given job is known
    /// never to return normally, so the job can be spawned in an even more
    /// lightweight manner.
    /// </summary>
    let inline server xJ = NativeHopac.server (Job.toHopac xJ)

    /// <summary>
    /// Creates a promise whose value is computed lazily with the given job when
    /// an attempt is made to read the promise.
    /// </summary>
    let inline memo x = NativeHopac.memo (Job.toHopac x)
    
    
    /// <summary>
    /// Creates an alternative that, after instantiation, becomes available after
    /// the specified time span.
    /// </summary>
    let inline timeOut ts = NativeHopac.timeOut ts |> Alt

    /// <summary>
    /// <c>timeOutMillis n</c> is equivalent to <c>timeOut &lt;&lt; TimeSpan.FromMilliseconds
    /// &lt;| float n</c>.
    /// </summary>
    let inline timeOutMillis n = NativeHopac.timeOutMillis n |> Alt
