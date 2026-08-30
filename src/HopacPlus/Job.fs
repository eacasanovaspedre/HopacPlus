namespace HopacPlus

open System
open System.Security.Cryptography
open System.Threading.Tasks
open Hopac

module HopacJob = Hopac.Job

/// <summary>A Job wrapped so FSharpPlus can resolve map / bind / monad.</summary>
[<Struct>]
type Job<'T> =
    | Job of HopacJob<'T>

    static member ToHopac(Job j) : HopacJob<'T> = j

/// <summary>Operations on jobs.</summary>
module Job =
    let inline toHopac (x: ^a) : #HopacJob<'t> = (^a: (static member ToHopac: 'a -> #HopacJob<'t>) x)

    let inline toHopacF ([<InlineIfLambda>] f) x = toHopac (f x)

    let inline toJob x = Job (toHopac x)

    /// <summary>
    /// Creates a job with the given result.  See also: lift, thunk, unit.
    /// </summary>
    let inline result x = x |> HopacJob.result |> Job

    /// <summary>
    /// Returns a job that does nothing and returns <c>()</c>.  <c>unit ()</c> is an
    /// optimized version of <c>result ()</c>.
    /// </summary>
    let inline unit () = () |> HopacJob.unit |> Job

    /// <summary>
    /// Creates a job that invokes the given thunk to compute the result of the
    /// job.  <c>thunk u2x</c> is equivalent to <c>result () &gt;&gt;- u2x</c>.
    /// </summary>
    let inline thunk ([<InlineIfLambda>] u2x) = u2x |> HopacJob.thunk |> Job

    /// <summary>
    /// Creates a job that calls the given function with the given value to
    /// compute the result of the job.  <c>lift x2y x</c> is equivalent to <c>result x
    /// &gt;&gt;- x2y</c>.  Note that <c>x2y x |&gt; result</c> is different.
    /// </summary>
    let inline lift ([<InlineIfLambda>] x2y) x = x |> HopacJob.lift x2y |> Job

    /// <summary>
    /// Creates a job that immediately terminates the current job.
    /// </summary>
    let inline abort () = () |> HopacJob.abort |> Job

    /// <summary>
    /// Creates a job that has the effect of raising the specified exception.
    /// <c>raises e</c> is equivalent to <c>Job.delayWith raise e</c>.
    /// </summary>
    let inline raises e = e |> HopacJob.raises |> Job

    /// <summary>
    /// Creates a job that runs the given job and maps the result of the job with
    /// the given function.  This is the same as <c>&gt;&gt;-</c> with the arguments flipped.
    /// </summary>
    let inline map ([<InlineIfLambda>] x2y: 'x -> 'y) (x: '``Job<'x>``) = x |> toHopac |> HopacJob.map x2y |> Job

    /// <summary>
    /// Creates a job that first runs the given job and then passes the result of
    /// that job to the given function to build another job which will then be
    /// run.  This is the same as <c>&gt;&gt;=</c> with the arguments flipped.
    /// </summary>
    let inline bind ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) (x: '``Job<'x>``): Job<'y> =
        x |> toHopac |> HopacJob.bind (toHopacF x2yJ) |> Job

    /// <summary>
    /// Creates a job that calls the given function to build a job that will then
    /// be run.  <c>delay u2xJ</c> is equivalent to <c>result () &gt;&gt;= u2xJ</c>.
    /// </summary>
    let inline delay ([<InlineIfLambda>] u2xJ: unit -> '``Job<'b>``) = u2xJ |> toHopacF |> HopacJob.delay |> Job

    /// <summary>
    /// Creates a job that calls the given function with the given value to build
    /// a job that will then be run.  <c>delayWith x2yJ x</c> is equivalent to <c>result
    /// x &gt;&gt;= x2yJ</c>.
    /// </summary>
    let inline delayWith ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) x : Job<'y> =
        x |> HopacJob.delayWith (toHopacF x2yJ) |> Job

    /// <summary>
    /// <c>join xJJ</c> is equivalent to <c>bind id xJJ</c>.
    /// </summary>
    let inline join (x: '``Job<Job<'x>>``) : Job<'x> = x |> toHopac |> HopacJob.map toHopac |> HopacJob.join |> Job //TODO: review this implementation

    /// <summary>
    /// <c>x2yJ |&gt; apply xJ</c> is equivalent to <c>x2yJ &gt;&gt;= fun x2y -&gt; xJ &gt;&gt;- x2y</c>.
    /// </summary>
    let inline apply (x: '``Job<'x>``) (x2yJ: '``Job<'x -> 'y>``) : Job<'y> =
        Job (HopacJob.apply (toHopac x) (toHopac x2yJ))

    /// <summary>
    /// Creates a job like the given job except that the result of the job will be
    /// <c>()</c>.  <c>Ignore xJ</c> is equivalent to <c>xJ &gt;&gt;- ignore</c>.
    /// </summary>
    let inline Ignore (x: '``Job<_>``) = x |> toHopac |> HopacJob.Ignore |> Job

    /// <summary>
    /// Implements the <c>try-in-unless</c> exception handling construct for jobs.
    /// Both of the continuation jobs <c>'x -&gt; Job&lt;'y&gt;</c>, for success, and <c>exn -&gt;
    /// Job&lt;'y&gt;</c>, for failure, are invoked from a tail position.  See also:
    /// <c>tryInDelay</c>.
    /// </summary>
    let inline tryIn
        (x: '``Job<'x>``)
        ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``)
        ([<InlineIfLambda>] e2yJ: exn -> '``Job<'y>``)
        : Job<'y> =
        HopacJob.tryIn (toHopac x) (toHopacF x2yJ) (toHopacF e2yJ) |> Job

    /// <summary>
    /// Implements the <c>try-in-unless</c> exception handling construct for jobs.
    /// Both of the continuation jobs <c>'x -&gt; Job&lt;'y&gt;</c>, for success, and <c>exn -&gt;
    /// Job&lt;'y&gt;</c>, for failure, are invoked from a tail position.  <c>tryInDelay u2xJ
    /// x2yJ e2yJ</c> is equivalent to <c>tryIn (delay u2xJ) x2yJ e2yJ</c>.
    /// </summary>
    let inline tryInDelay
        ([<InlineIfLambda>] u2xJ: unit -> '``Job<'x>``)
        ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``)
        ([<InlineIfLambda>] e2yJ: exn -> '``Job<'y>``)
        : Job<'y> =
        HopacJob.tryInDelay (toHopacF u2xJ) (toHopacF x2yJ) (toHopacF e2yJ) |> Job

    /// <summary>
    /// Implements the try-with exception handling construct for jobs.
    /// </summary>
    let inline tryWith (x: '``Job<'x>``) ([<InlineIfLambda>] e2xJ: exn -> '``Job<'x>``) : Job<'x> =
        HopacJob.tryWith (toHopac x) (toHopacF e2xJ) |> Job

    /// <summary>
    /// Implements the try-with exception handling construct for jobs.
    /// </summary>
    let inline tryWithDelay
        ([<InlineIfLambda>] u2xJ: unit -> '``Job<'x>``)
        ([<InlineIfLambda>] e2xJ: exn -> '``Job<'x>``)
        : Job<'x> =
        HopacJob.tryWithDelay (toHopacF u2xJ) (toHopacF e2xJ) |> Job

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a function, is executed after
    /// the job has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyFun (x: '``Job<'x>``) ([<InlineIfLambda>] u2u) : Job<'x> =
        HopacJob.tryFinallyFun (toHopac x) u2u |> Job

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a function, is executed after
    /// the job has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyFunDelay ([<InlineIfLambda>] u2xJ: unit -> '``Job<'x>``) ([<InlineIfLambda>] u2u) : Job<'x> =
        HopacJob.tryFinallyFunDelay (toHopacF u2xJ) u2u |> Job

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a job, is executed after the job
    /// has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyJob (x: '``Job<'x>``) (uJ: '``Job<unit>``) : Job<'x> =
        HopacJob.tryFinallyJob (toHopac x) (toHopac uJ) |> Job

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a job, is executed after the job
    /// has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyJobDelay ([<InlineIfLambda>] u2xJ: unit -> '``Job<'x>``) (uJ: '``Job<unit>``) : Job<'x> =
        HopacJob.tryFinallyJobDelay (toHopacF u2xJ) (toHopac uJ) |> Job

    /// <summary>
    /// Creates a job that runs the given job and results in either the ordinary
    /// result of the job or the exception raised by the job.
    /// </summary>
    let inline catch (x: '``Job<'x>``) : Job<Choice<'x, _>> = HopacJob.catch (toHopac x) |> Job

    /// <summary>
    /// Implements the <c>use</c> construct for jobs.  The <c>Dispose</c> method of the
    /// given disposable object is called after running the job constructed with
    /// the disposable object.  See also: abort, usingAsync.
    /// </summary>
    let inline using resource ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) : Job<'y> =
        HopacJob.using resource (toHopacF x2yJ) |> Job

    /// <summary>
    /// Implements an experimental <c>use</c> like construct for asynchronously
    /// disposable resources.  The <c>DisposeAsync</c> method of the asynchronously
    /// disposable resource is called to construct a job later used to
    /// dispose the resource after the constructed job returns.  See also:
    /// abort, using.
    /// </summary>
    let inline usingAsync resource ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) : Job<'y> =
        HopacJob.usingAsync resource (toHopacF x2yJ) |> Job

    /// <summary>
    /// <c>useIn x2yJ x</c> is equivalent to <c>using x x2yJ</c> and can be more convenient
    /// to use in pipelines (i.e. <c>x |&gt; useIn x2yJ</c>).
    /// </summary>
    let inline useIn ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) resource : Job<'y> =
        resource |> HopacJob.useIn (toHopacF x2yJ) |> Job

    /// <summary>
    /// Creates a job that runs the given job sequentially the given number of
    /// times.
    /// </summary>
    let inline forN n (uJ: '``Job<unit>``) = uJ |> toHopac |> HopacJob.forN n |> Job

    /// <summary>
    /// <c>forNIgnore n xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; forN n</c>.
    /// </summary>
    let inline forNIgnore n (xJ: '``Job<'x>``) = xJ |> toHopac |> HopacJob.forNIgnore n |> Job

    /// <summary>
    /// <c>forUpTo lo hi i2uJ</c> creates a job that sequentially iterates from <c>lo</c> to
    /// <c>hi</c> (inclusive) and calls the given function to construct jobs that
    /// will be executed.
    /// </summary>
    let inline forUpTo lo hi ([<InlineIfLambda>] i2uJ: int -> '``Job<unit>``) =
        i2uJ |> toHopacF |> HopacJob.forUpTo lo hi |> Job

    /// <summary>
    /// <c>forUpToIgnore lo hi i2xJ</c> is equivalent to <c>forUpTo lo hi (i2xJ &gt;&gt;
    /// Job.Ignore)</c>.
    /// </summary>
    let inline forUpToIgnore lo hi ([<InlineIfLambda>] i2xJ: int -> '``Job<'x>``) =
        i2xJ |> toHopacF |> HopacJob.forUpToIgnore lo hi |> Job

    /// <summary>
    /// <c>forDownTo hi lo i2uJ</c> creates a job that sequentially iterates from <c>hi</c>
    /// to <c>lo</c> (inclusive) and calls the given function to construct jobs that
    /// will be executed.
    /// </summary>
    let inline forDownTo hi lo ([<InlineIfLambda>] i2uJ: int -> '``Job<unit>``) =
        i2uJ |> toHopacF |> HopacJob.forDownTo hi lo |> Job

    /// <summary>
    /// <c>forDownToIgnore hi lo i2xJ</c> is equivalent to <c>forDownTo hi lo (i2xJ &gt;&gt;
    /// Job.Ignore)</c>.
    /// </summary>
    let inline forDownToIgnore hi lo ([<InlineIfLambda>] i2xJ: int -> '``Job<'x>``) =
        i2xJ |> toHopacF |> HopacJob.forDownToIgnore hi lo |> Job

    /// <summary>
    /// <c>whileDo u2b uJ</c> creates a job that sequentially executes the <c>uJ</c> job as
    /// long as <c>u2b ()</c> returns <c>true</c>.  See also: whileDoDelay.
    /// </summary>
    let inline whileDo ([<InlineIfLambda>] u2b) (uJ: '``Job<unit>``) = uJ |> toHopac |> HopacJob.whileDo u2b |> Job

    /// <summary>
    /// <c>whileDoDelay u2b u2xJ</c> creates a job that sequentially constructs a job
    /// with <c>u2xJ</c> and executes it as long as <c>u2b ()</c> returns <c>true</c>.
    /// </summary>
    let inline whileDoDelay ([<InlineIfLambda>] u2b) ([<InlineIfLambda>] u2xJ: unit -> '``Job<'x>``) =
        u2xJ |> toHopacF |> HopacJob.whileDoDelay u2b |> Job

    /// <summary>
    /// <c>whileDoIgnore u2b xJ</c> creates a job that sequentially executes the <c>xJ</c>
    /// job as long as <c>u2b ()</c> returns <c>true</c>.  <c>whileDoIgnore u2b xJ</c> is
    /// equivalent to <c>Job.Ignore xJ |&gt; whileDo u2b</c>.
    /// </summary>
    let inline whileDoIgnore ([<InlineIfLambda>] u2b) (xJ: '``Job<'x>``) =
        xJ |> toHopac |> HopacJob.whileDoIgnore u2b |> Job

    /// <summary>
    /// <c>whenDo b uJ</c> is equivalent to <c>if b then uJ else Job.unit ()</c>.
    /// </summary>
    let inline whenDo b (uJ: '``Job<unit>``) = uJ |> toHopac |> HopacJob.whenDo b |> Job

    /// <summary>
    /// Creates a job that repeats the given job indefinitely.  See also:
    /// foreverServer, iterate.
    /// </summary>
    let inline forever (uJ: '``Job<unit>``) = uJ |> toHopac |> HopacJob.forever |> Job

    /// <summary>
    /// <c>foreverIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; forever</c>.
    /// </summary>
    let inline foreverIgnore (xJ: '``Job<'x>``) = xJ |> toHopac |> HopacJob.foreverIgnore |> Job

    /// <summary>
    /// Creates a job that indefinitely iterates the given job constructor
    /// starting with the given value.  See also: iterateServer, forever.
    /// </summary>
    let inline iterate x ([<InlineIfLambda>] x2xJ: 'x -> '``Job<'x>``) : Job<'x> =
        x2xJ |> toHopacF |> HopacJob.iterate x |> Job

    /// <summary>
    /// Creates a job that starts a separate server job that repeats the given job
    /// indefinitely.  <c>foreverServer xJ</c> is equivalent to <c>forever xJ |&gt; server</c>.
    /// </summary>
    let inline foreverServer (uJ: '``Job<unit>``) = uJ |> toHopac |> HopacJob.foreverServer |> Job

    /// <summary>
    /// Creates a job that starts a separate server job that indefinitely iterates
    /// the given job constructor starting with the given value.  <c>iterateServer x
    /// x2xJ</c> is equivalent to <c>iterate x x2xJ |&gt; server</c>.
    /// </summary>
    let inline iterateServer x ([<InlineIfLambda>] x2xJ: 'x -> '``Job<'x>``) =
        x2xJ |> toHopacF |> HopacJob.iterateServer x |> Job

    /// <summary>
    /// Creates a job that runs all the jobs in sequence and returns a list of
    /// the results.  See also: seqIgnore, conCollect, Seq.mapJob.
    /// </summary>
    let inline seqCollect (xJs: '``Job<'x>`` seq) : Job<ResizeArray<'x>> =
        xJs |> Seq.map toHopac |> HopacJob.seqCollect |> Job

    /// <summary>
    /// Creates a job that runs all the jobs as separate concurrent jobs and
    /// returns a list of the results.  See also: conIgnore, seqCollect,
    /// Seq.Con.mapJob.
    /// Note that when multiple jobs raise exceptions, then the created job raises
    /// an AggregateException.
    /// Note that this is not optimal for fine-grained parallel execution.
    /// </summary>
    let inline conCollect (xJs: '``Job<'x>`` seq) : Job<ResizeArray<'x>> =
        xJs |> Seq.map toHopac |> HopacJob.conCollect |> Job

    /// <summary>
    /// Creates a job that runs all the jobs in sequence.  The results of the
    /// jobs are ignored.  See also: seqCollect, conIgnore, Seq.iterJob.
    /// </summary>
    let inline seqIgnore (xJs: '``Job<'x>`` seq) = xJs |> Seq.map toHopac |> HopacJob.seqIgnore |> Job

    /// <summary>
    /// Creates a job that runs all the jobs as separate concurrent jobs and
    /// then waits for all the jobs to finish.  The results of the jobs are
    /// ignored.  See also: conCollect, seqIgnore, Seq.Con.iterJob.
    /// Note that when multiple jobs raise exceptions, then the created job raises
    /// an AggregateException.
    /// Note that this is not optimal for fine-grained parallel execution.
    /// </summary>
    let inline conIgnore (xJs: '``Job<'x>`` seq) = xJs |> Seq.map toHopac |> HopacJob.conIgnore |> Job

    /// <summary>
    /// Creates a job that performs the asynchronous operation defined by the
    /// given pair of <c>doBegin</c> and <c>doEnd</c> operations.  See also:
    /// Alt.fromBeginEnd.
    /// </summary>
    let inline fromBeginEnd doBegin doEnd = HopacJob.fromBeginEnd doBegin doEnd |> Job

    /// <summary>
    /// <c>fromEndBegin doEnd doBegin</c> is equivalent to <c>fromBeginEnd doBegin
    /// doEnd</c>.
    /// </summary>
    let inline fromEndBegin doEnd doBegin = HopacJob.fromEndBegin doEnd doBegin |> Job

    /// <summary>
    /// Creates a job that starts an asynchronous operation by calling the given
    /// function with success and failure continuations of which exactly one must
    /// be called once.
    /// </summary>
    let inline fromContinuations kont = HopacJob.fromContinuations kont |> Job

    /// <summary>
    /// Creates a job that queues the given thunk to execute on the system
    /// ThreadPool and then waits for the result of the thunk.
    /// </summary>
    let inline onThreadPool ([<InlineIfLambda>] u2x) = HopacJob.onThreadPool u2x |> Job

    /// <summary>
    /// Creates a job that starts the given async operation and waits for it to
    /// complete.  See also: Alt.fromAsync.
    /// </summary>
    let inline fromAsync xA = HopacJob.fromAsync xA |> Job

    /// <summary>
    /// Creates an async operation that starts the given job and waits for it to
    /// complete.
    /// </summary>
    let inline toAsync x = HopacJob.toAsync (toHopac x)

    /// <summary>
    /// <c>bindAsync x2yJ xA</c> is equivalent to <c>fromAsync xA &gt;&gt;= x2yJ</c>.
    /// </summary>
    let inline bindAsync ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) xA : Job<'y> =
        xA |> HopacJob.bindAsync (toHopacF x2yJ) |> Job

    /// <summary>
    /// Creates a job that calls the given function to start a task and waits for
    /// it to complete.  See also: Alt.fromTask.
    /// </summary>
    let inline fromTask ([<InlineIfLambda>] u2xT) = HopacJob.fromTask u2xT |> Job

    /// <summary>
    /// Creates a job that calls the given function to start a task and waits for
    /// it to complete.  See also: Alt.fromUnitTask.
    /// </summary>
    let inline fromUnitTask ([<InlineIfLambda>] u2uT) = HopacJob.fromUnitTask u2uT |> Job

    /// <summary>
    /// <c>liftTask x2yT</c> is equivalent to <c>fun x -&gt; fromTask &lt;| fun () -&gt; x2yT x</c>.
    /// </summary>
    let inline liftTask ([<InlineIfLambda>] x2yT) x = HopacJob.liftTask x2yT x |> Job

    /// <summary>
    /// <c>liftUnitTask x2uT</c> is equivalent to <c>fun x -&gt; fromUnitTask &lt;| fun () -&gt;
    /// x2uT x</c>.
    /// </summary>
    let inline liftUnitTask ([<InlineIfLambda>] x2uT) x = HopacJob.liftUnitTask x2uT x |> Job

    /// <summary>
    /// Creates a job that waits for the given task to finish and then returns the
    /// result of the task.  Note that this does not start the task.  Make sure
    /// that the task is started correctly.  Exceptions thrown during task
    /// initialization may not be caught. Prefer fromTask or liftTask.
    /// </summary>
    let inline awaitTask (xT: Task<_>) = HopacJob.awaitTask xT |> Job

    /// <summary>
    /// Creates a job that waits until the given task finishes.  Note that this
    /// does not start the task.  Make sure that the task is started correctly.
    /// Exceptions thrown during task initialization may not be caught. Prefer
    /// fromUnitTask or liftUnitTask.
    /// </summary>
    let inline awaitUnitTask (uT: Task) = HopacJob.awaitUnitTask uT |> Job

    /// <summary>
    /// <c>bindTask x2yJ xT</c> is equivalent to <c>awaitTask xT &gt;&gt;= x2yJ</c>.
    /// Exceptions thrown during task initialization may not be caught. Prefer
    /// fromTask or liftTask to convert the task to a Job and use Job.bind.
    /// </summary>
    let inline bindTask ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) xT : Job<'y> =
        HopacJob.bindTask (toHopacF x2yJ) xT |> Job

    /// <summary>
    /// <c>bindUnitTask u2xJ uT</c> is equivalent to <c>awaitUnitTask uT &gt;&gt;= u2xJ</c>.
    /// Exceptions thrown during task initialization may not be caught. Prefer
    /// fromUnitTask or liftUnitTask to convert the task to a Job and
    /// use Job.bind.
    /// </summary>
    let inline bindUnitTask ([<InlineIfLambda>] u2xJ: unit -> '``Job<'y>``) uT : Job<'y> =
        HopacJob.bindUnitTask (toHopacF u2xJ) uT |> Job

    /// <summary>
    /// Creates a job that immediately starts running the given job as a separate
    /// concurrent job.  Use Promise.start if you need to be able to get the
    /// result.  Use Job.server if the job never returns normally.  See also:
    /// Job.queue, Proc.start.
    /// </summary>
    let inline start (uJ: '``Job<unit>``) = HopacJob.start (toHopac uJ) |> Job

    /// <summary>
    /// Creates a job that immediately starts running the given job as a separate
    /// concurrent job.  <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt;
    /// start</c>.
    /// </summary>
    let inline startIgnore (xJ: '``Job<'x>``) = HopacJob.startIgnore (toHopac xJ) |> Job

    /// <summary>
    /// Creates a job that schedules the given job to be run as a separate
    /// concurrent job.  Use Promise.queue if you need to be able to get the
    /// result.  See also: Proc.queue.
    /// </summary>
    let inline queue (uJ: '``Job<unit>``) = HopacJob.queue (toHopac uJ) |> Job

    /// <summary>
    /// Creates a job that schedules the given job to be run as a separate
    /// concurrent job.  <c>queueIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt;
    /// queue</c>.
    /// </summary>
    let inline queueIgnore (xJ: '``Job<'x>``) = HopacJob.queueIgnore (toHopac xJ) |> Job

    /// <summary>
    /// Creates a job that immediately starts running the given job as a separate
    /// concurrent job like start, but the given job is known never to return
    /// normally, so the job can be spawned in an even more lightweight manner.
    /// </summary>
    let inline server (xJ: '``Job<'x>``) = HopacJob.server (toHopac xJ) |> Job

    /// <summary>
    /// Given a job, creates a new job that behaves exactly like the given job,
    /// except that the new job obviously cannot be directly downcast to the
    /// underlying type of the given job.  This operation is provided for
    /// debugging purposes.  You can always break abstractions using reflection.
    /// See also: Alt.paranoid.
    /// </summary>
    let inline paranoid (xJ: '``Job<'x>``) : Job<'x> = HopacJob.paranoid (toHopac xJ) |> Job

    /// <summary>
    /// Operations on the built-in pseudo random number generator (PRNG) of Hopac.
    /// </summary>
    module Random =
        /// <summary>
        /// Returns a job that generates a pseudo random 64-bit unsigned integer.
        /// <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline get () = HopacJob.Random.get () |> Job

        /// <summary>
        /// <c>map r2x</c> is equivalent to <c>bind (r2x &gt;&gt; result)</c>.
        /// </summary>
        let inline map ([<InlineIfLambda>] r2x) = HopacJob.Random.map r2x |> Job

        /// <summary>
        /// <c>bind r2xJ</c> creates a job that calls the given job constructor with a
        /// pseudo random 64-bit unsigned integer.
        /// </summary>
        let inline bind ([<InlineIfLambda>] r2xJ) = HopacJob.Random.bind (toHopacF r2xJ) |> Job

    /// <summary>
    /// Operations on the built-in pseudo random number generator (PRNG) of Hopac.
    /// </summary>
    module RandomCrypto =
        /// <summary>
        /// Returns a job that generates a cryptographically strong random integer in the range [Int32.MinValue, Int32.MaxValue).
        /// <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline get () =
            thunk (fun () -> RandomNumberGenerator.GetInt32 (Int32.MinValue, Int32.MaxValue))

        /// <summary>
        /// Returns a job that generates a cryptographically strong random integer in the range [0, Int32.MaxValue).
        /// <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline getNonNegative () = thunk (fun () -> RandomNumberGenerator.GetInt32 Int32.MaxValue)

        /// <summary>
        /// Returns a job that generates a cryptographically strong random integer in the range [0, toExclusive).
        /// <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline getNonNegativeTo toExclusive = thunk (fun () -> RandomNumberGenerator.GetInt32 toExclusive)

        /// <summary>
        /// Returns a job that generates a cryptographically strong random integer in the range [toInclusive, toExclusive).
        /// <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline getFromTo toInclusive toExclusive =
            thunk (fun () -> RandomNumberGenerator.GetInt32 (toInclusive, toExclusive))

    /// <summary>Operations for dealing with the scheduler.</summary>
    module Scheduler =
        /// <summary>
        /// <c>bind s2xJ</c> creates a job that calls the given job constructor with the
        /// scheduler under which the job is being executed.  bind allows
        /// interfacing Hopac with existing asynchronous operations that do not fall
        /// into a pattern that is already supported explicitly.
        /// </summary>
        let inline bind ([<InlineIfLambda>] s2xJ: Scheduler -> '``Job<'x>``) : Job<'x> =
            HopacJob.Scheduler.bind (toHopacF s2xJ) |> Job

        /// <summary>
        /// Returns a job that returns the scheduler under which the job is being
        /// run.  <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline get () = HopacJob.Scheduler.get () |> Job

        /// <summary>
        /// Returns a job that ensures that the immediately following operation will
        /// be executed on a Hopac worker thread.
        /// </summary>
        let inline switchToWorker () = HopacJob.Scheduler.switchToWorker () |> Job

        /// <summary>
        /// <c>isolate u2x</c> is like <c>thunk u2x</c>, but it is ensured that the blocking
        /// invocation of <c>u2x</c> does not prevent scheduling of other work.
        /// </summary>
        let inline isolate ([<InlineIfLambda>] u2x) = HopacJob.Scheduler.isolate u2x |> Job

    /// <summary>Operations on the global scheduler.</summary>
    module Global =
        /// <summary>
        /// Starts running the given job on the global scheduler and then blocks the
        /// current thread waiting for the job to either return successfully or
        /// fail.
        /// </summary>
        let inline run x = run (toHopac x)

        /// <summary>
        /// Starts running the given job on the global scheduler but does not wait
        /// for the job to finish.  See also: queue, server.
        /// </summary>
        let inline start uJ = Hopac.start (toHopac uJ)

        /// <summary>
        /// Starts running the given job on the global scheduler but does not wait
        /// for the job to finish.  <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ
        /// |&gt; start</c>.
        /// </summary>
        let inline startIgnore (xJ: '``Job<'x>``) = Hopac.startIgnore (toHopac xJ)

        /// <summary>
        /// Starts running the given job on the global scheduler but does not wait
        /// for the job to finish.  Upon the failure or success of the job, one of
        /// the given actions is called once.
        /// </summary>
        let inline startWithActions e2u (x2u: 'x -> unit) (x: '``Job<'x>``) = startWithActions e2u x2u (toHopac x)

        /// <summary>
        /// Queues the job for execution on the global scheduler.  See also:
        /// start, server.
        /// </summary>
        let inline queue (uJ: '``Job<unit>``) = Hopac.queue (toHopac uJ)

        /// <summary>
        /// Queues the job for execution on the global scheduler.  <c>queueIgnore xJ</c>
        /// is equivalent to <c>Job.Ignore xJ |&gt; queue</c>.
        /// </summary>
        let inline queueIgnore (xJ: '``Job<'x>``) = Hopac.queueIgnore (toHopac xJ)

        /// <summary>
        /// Like Job.Global.start, but the given job is known never to return
        /// normally, so the job can be spawned in an even more lightweight manner.
        /// </summary>
        let inline server (xJ: '``Job<'x>``) = Hopac.server (toHopac xJ)

type Job<'x> with
    static member inline Return(x: 'T) = Job.result x
    static member inline Map(x, [<InlineIfLambda>] f) = Job.map f x

    static member inline (>>=)(x, [<InlineIfLambda>] f) = Job.bind f x

    static member inline (>>=)(x: HopacJob<'T>, [<InlineIfLambda>] f) = HopacJob.bind (Job.toHopacF f) x |> Job

    static member inline (>>=)(x: Async<'T>, [<InlineIfLambda>] f) = Job.bindAsync f x

    static member inline (>>=)(x: Task<'T>, [<InlineIfLambda>] f) = Job.bindTask f x

    static member inline (>>=)(x: Task, [<InlineIfLambda>] f) = Job.bindUnitTask f x

    static member inline (<*>)(f, x) = Job.apply f x

    static member inline Join(x) = Job.join x

    static member inline Delay([<InlineIfLambda>] f) = Job.delay f

    static member inline TryWith(computation, [<InlineIfLambda>] handler) = Job.tryWith computation handler

    static member inline TryFinally(computation, [<InlineIfLambda>] compensation: unit -> unit) =
        Job.tryFinallyFun computation compensation

    static member inline Using(resource: #IDisposable, [<InlineIfLambda>] body) = Job.using resource body
