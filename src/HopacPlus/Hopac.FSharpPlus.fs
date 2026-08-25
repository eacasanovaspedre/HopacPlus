#nowarn "1215" // static operators on type extensions are for F#+ SRTP, not F# infix

/// <summary>
/// FSharpPlus instances for Hopac, plus JobM/AltM mirrors of Hopac's Job, Alt,
/// and top-level Hopac module.
/// </summary>
[<AutoOpen>]
module Hopac.FSharpPlus

open System
open Hopac

/// <summary>A Job wrapped so FSharpPlus can resolve map / bind / monad.</summary>
[<Struct>]
type JobM<'T> =
    | JobM of Job<'T>

    static member inline ToJob(JobM j) : Job<'T> = j

    static member inline Return(x: 'T) = JobM(Job.result x)
    static member inline Map(JobM x, [<InlineIfLambda>] f) = JobM(Job.map f x)

    static member inline (>>=)(JobM x, [<InlineIfLambda>] f: 'T -> JobM<'U>) =
        JobM(Job.bind (fun a -> let (JobM y) = f a in y) x)

    static member inline (>>=)(x: Job<'T>, [<InlineIfLambda>] f: 'T -> JobM<'U>) =
        JobM(Job.bind (fun a -> let (JobM y) = f a in y) x)

    static member inline (>>=)(x: Async<'T>, [<InlineIfLambda>] f: 'T -> JobM<'U>) =
        JobM(Job.bindAsync (fun a -> let (JobM y) = f a in y) x)

    static member inline (>>=)(x: Threading.Tasks.Task<'T>, [<InlineIfLambda>] f: 'T -> JobM<'U>) =
        JobM(Job.bindTask (fun a -> let (JobM y) = f a in y) x)

    static member inline (>>=)(x: Threading.Tasks.Task, [<InlineIfLambda>] f: unit -> JobM<'U>) =
        JobM(Job.bindUnitTask (fun () -> let (JobM y) = f () in y) x)

    static member inline (<*>)(JobM f, JobM x) = JobM(Job.apply x f)
    static member inline Join(JobM x) = JobM(Job.bind (fun (JobM y) -> y) x)

    static member inline Delay([<InlineIfLambda>] f: unit -> JobM<'T>) =
        JobM(Job.delay (fun () -> let (JobM j) = f () in j))

    static member inline TryWith(JobM computation, [<InlineIfLambda>] handler: exn -> JobM<'T>) =
        JobM(Job.tryWith computation (fun e -> let (JobM j) = handler e in j))

    static member inline TryFinally(JobM computation, [<InlineIfLambda>] compensation: unit -> unit) =
        JobM(Job.tryFinallyFun computation compensation)

    static member inline Using(resource: #IDisposable, [<InlineIfLambda>] body: _ -> JobM<'U>) =
        JobM(Job.using resource (fun r -> let (JobM j) = body r in j))

/// <summary>An Alt wrapped so FSharpPlus can resolve map / bind / empty / (<|>).</summary>
[<Struct>]
type AltM<'T> =
    | AltM of Alt<'T>

    static member inline ToJob(AltM a) : Job<'T> = a :> Job<'T>

    static member inline Return(x: 'T) = AltM(Alt.always x)
    static member inline Map(AltM x, [<InlineIfLambda>] f) = AltM(Alt.afterFun f x)

    static member inline (>>=)(AltM x, [<InlineIfLambda>] f: 'T -> AltM<'U>) =
        AltM(Alt.afterJob (fun a -> let (AltM y) = f a in y) x)

    static member inline Delay([<InlineIfLambda>] f: unit -> AltM<'T>) =
        AltM(Alt.prepareFun (fun () -> let (AltM a) = f () in a))

    static member inline Empty() : AltM<'T> = AltM(Alt.never ())
    static member inline get_Empty() : AltM<'T> = AltM(Alt.never ())
    static member inline (<|>)(AltM x, AltM y) = AltM(Alt.choose [ x; y ])

let inline private unJ (JobM x) = x
let inline private mkJ x = JobM x
let inline private unA (AltM x) = x
let inline private mkA x = AltM x

// Only look at ^a. A witness ToJob(Job) poisons SRTP: (^a or W) commits to
// the identity Job -> Job and then rejects JobM/AltM. Job is already a Job;
// wrap with JobM.ofJob or call Hopac.run / Job.bind directly.
let inline private toHopacJob (x: ^a) : Job<'t> =
    (^a: (static member ToJob: ^a -> Job<'t>) x)

let inline private unwrapJ ([<InlineIfLambda>] f) a = toHopacJob (f a)

let inline private unwrapA ([<InlineIfLambda>] f) a =
    let (AltM y) = f a
    y

type JobsOf =
    static member Of(xs: seq<JobM<'T>>) : seq<Job<'T>> =
        match xs with
        | :? array<JobM<'T>> as arr ->
            let ys = Array.zeroCreate arr.Length

            for i = 0 to arr.Length - 1 do
                ys[i] <- unJ arr[i]

            ys :> seq<_>
        | _ ->
            let ra = ResizeArray()

            for JobM j in xs do
                ra.Add j

            ra :> seq<_>

    static member Of(xs: seq<AltM<'T>>) : seq<Job<'T>> =
        match xs with
        | :? array<AltM<'T>> as arr ->
            let ys = Array.zeroCreate arr.Length

            for i = 0 to arr.Length - 1 do
                ys[i] <- unA arr[i] :> Job<'T>

            ys :> seq<_>
        | _ ->
            let ra = ResizeArray()

            for AltM a in xs do
                ra.Add(a :> Job<'T>)

            ra :> seq<_>

let inline private jobsOf xs =
    ((^a or JobsOf): (static member Of: ^a -> seq<Job<_>>) xs)

type JobM<'T> with
    static member inline (>>=)(x: AltM<'U>, [<InlineIfLambda>] f: 'U -> JobM<'T>) =
        JobM(Job.bind (fun a -> let (JobM y) = f a in y) (unA x :> Job<_>))

    static member inline (<*>)(JobM f, AltM x) = JobM(Job.apply (x :> Job<_>) f)
    static member inline (<*>)(AltM f, JobM x) = JobM(Job.apply x (f :> Job<_>))
    static member inline (<*>)(AltM f, AltM x) = JobM(Job.apply (x :> Job<_>) (f :> Job<_>))

let inline private altsOf (xs: #seq<AltM<'T>>) =
    match xs :> seq<_> with
    | :? array<AltM<'T>> as arr ->
        let ys = Array.zeroCreate arr.Length

        for i = 0 to arr.Length - 1 do
            ys[i] <- unA arr[i]

        ys :> seq<_>
    | _ ->
        let ra = ResizeArray()

        for AltM a in xs do
            ra.Add a

        ra :> seq<_>

/// <summary>Operations on jobs.</summary>
module JobM =
    open System.Threading.Tasks

    /// <summary>Wrap a Hopac Job.</summary>
    let inline ofJob x = mkJ x

    /// <summary>Unwrap to a Hopac Job.</summary>
    let inline toJob x = toHopacJob x

    /// <summary>View a JobM or AltM as JobM. Wrap a Hopac Job with <c>ofJob</c>.</summary>
    let inline ofJobLike x = mkJ (toHopacJob x)

    /// <summary>
    /// Starts running the given job and then blocks the current thread waiting
    /// for the job to either return successfully or fail.  See also: start.
    /// </summary>
    let inline run x = run (toHopacJob x)

    /// <summary>
    /// Creates a job with the given result.  See also: lift, thunk, unit.
    /// </summary>
    let inline result x = mkJ (Job.result x)

    /// <summary>
    /// Returns a job that does nothing and returns <c>()</c>.  <c>unit ()</c> is an
    /// optimized version of <c>result ()</c>.
    /// </summary>
    let inline unit () = mkJ (Job.unit ())

    /// <summary>
    /// Creates a job that invokes the given thunk to compute the result of the
    /// job.  <c>thunk u2x</c> is equivalent to <c>result () &gt;&gt;- u2x</c>.
    /// </summary>
    let inline thunk ([<InlineIfLambda>] u2x) = mkJ (Job.thunk u2x)

    /// <summary>
    /// Creates a job that calls the given function with the given value to
    /// compute the result of the job.  <c>lift x2y x</c> is equivalent to <c>result x
    /// &gt;&gt;- x2y</c>.  Note that <c>x2y x |&gt; result</c> is not the same.
    /// </summary>
    let inline lift ([<InlineIfLambda>] x2y) x = mkJ (Job.lift x2y x)

    /// <summary>
    /// Creates a job that immediately terminates the current job.
    /// </summary>
    let inline abort () = mkJ (Job.abort ())

    /// <summary>
    /// Creates a job that has the effect of raising the specified exception.
    /// <c>raises e</c> is equivalent to <c>Job.delayWith raise e</c>.
    /// </summary>
    let inline raises e = mkJ (Job.raises e)

    /// <summary>
    /// Creates a job that runs the given job and maps the result of the job with
    /// the given function.  This is the same as <c>&gt;&gt;-</c> with the arguments flipped.
    /// </summary>
    let inline map ([<InlineIfLambda>] x2y) x = mkJ (Job.map x2y (toHopacJob x))

    /// <summary>
    /// Creates a job that first runs the given job and then passes the result of
    /// that job to the given function to build another job which will then be
    /// run.  This is the same as <c>&gt;&gt;=</c> with the arguments flipped.
    /// </summary>
    let inline bind ([<InlineIfLambda>] x2yJ) x = mkJ (Job.bind (unwrapJ x2yJ) (toHopacJob x))

    /// <summary>
    /// Creates a job that calls the given function to build a job that will then
    /// be run.  <c>delay u2xJ</c> is equivalent to <c>result () &gt;&gt;= u2xJ</c>.
    /// </summary>
    let inline delay ([<InlineIfLambda>] u2xJ) = mkJ (Job.delay (unwrapJ u2xJ))

    /// <summary>
    /// Creates a job that calls the given function with the given value to build
    /// a job that will then be run.  <c>delayWith x2yJ x</c> is equivalent to <c>result
    /// x &gt;&gt;= x2yJ</c>.
    /// </summary>
    let inline delayWith ([<InlineIfLambda>] x2yJ) x = mkJ (Job.delayWith (unwrapJ x2yJ) x)

    /// <summary>
    /// <c>join xJJ</c> is equivalent to <c>bind id xJJ</c>.
    /// </summary>
    let inline join x = mkJ (Job.bind (fun inner -> toHopacJob inner) (toHopacJob x))

    /// <summary>
    /// <c>x2yJ |&gt; apply xJ</c> is equivalent to <c>x2yJ &gt;&gt;= fun x2y -&gt; xJ &gt;&gt;- x2y</c>.
    /// </summary>
    let inline apply x x2yJ = mkJ (Job.apply (toHopacJob x) (toHopacJob x2yJ))

    /// <summary>
    /// Creates a job like the given job except that the result of the job will be
    /// <c>()</c>.  <c>Ignore xJ</c> is equivalent to <c>xJ &gt;&gt;- ignore</c>.
    /// </summary>
    let inline Ignore x = mkJ (Job.Ignore (toHopacJob x))

    /// <summary>
    /// Implements the <c>try-in-unless</c> exception handling construct for jobs.
    /// Both of the continuation jobs <c>'x -&gt; Job&lt;'y&gt;</c>, for success, and <c>exn -&gt;
    /// Job&lt;'y&gt;</c>, for failure, are invoked from a tail position.  See also:
    /// <c>tryInDelay</c>.
    /// </summary>
    let inline tryIn x ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] e2yJ) =
        mkJ (Job.tryIn (toHopacJob x) (unwrapJ x2yJ) (unwrapJ e2yJ))

    /// <summary>
    /// Implements the <c>try-in-unless</c> exception handling construct for jobs.
    /// Both of the continuation jobs <c>'x -&gt; Job&lt;'y&gt;</c>, for success, and <c>exn -&gt;
    /// Job&lt;'y&gt;</c>, for failure, are invoked from a tail position.  <c>tryInDelay u2xJ
    /// x2yJ e2yJ</c> is equivalent to <c>tryIn (delay u2xJ) x2yJ e2yJ</c>.
    /// </summary>
    let inline tryInDelay ([<InlineIfLambda>] u2xJ) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] e2yJ) =
        mkJ (Job.tryInDelay (unwrapJ u2xJ) (unwrapJ x2yJ) (unwrapJ e2yJ))

    /// <summary>
    /// Implements the try-with exception handling construct for jobs.
    /// </summary>
    let inline tryWith x ([<InlineIfLambda>] e2xJ) = mkJ (Job.tryWith (toHopacJob x) (unwrapJ e2xJ))

    /// <summary>
    /// Implements the try-with exception handling construct for jobs.
    /// </summary>
    let inline tryWithDelay ([<InlineIfLambda>] u2xJ) ([<InlineIfLambda>] e2xJ) =
        mkJ (Job.tryWithDelay (unwrapJ u2xJ) (unwrapJ e2xJ))

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a function, is executed after
    /// the job has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyFun x ([<InlineIfLambda>] u2u) = mkJ (Job.tryFinallyFun (toHopacJob x) u2u)

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a function, is executed after
    /// the job has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyFunDelay ([<InlineIfLambda>] u2xJ) ([<InlineIfLambda>] u2u) =
        mkJ (Job.tryFinallyFunDelay (unwrapJ u2xJ) u2u)

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a job, is executed after the job
    /// has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyJob x uJ = mkJ (Job.tryFinallyJob (toHopacJob x) (toHopacJob uJ))

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for jobs.  The given action, specified as a job, is executed after the job
    /// has been run, whether it fails or completes successfully.
    /// </summary>
    let inline tryFinallyJobDelay ([<InlineIfLambda>] u2xJ) uJ =
        mkJ (Job.tryFinallyJobDelay (unwrapJ u2xJ) (toHopacJob uJ))

    /// <summary>
    /// Creates a job that runs the given job and results in either the ordinary
    /// result of the job or the exception raised by the job.
    /// </summary>
    let inline catch x = mkJ (Job.catch (toHopacJob x))

    /// <summary>
    /// Implements the <c>use</c> construct for jobs.  The <c>Dispose</c> method of the
    /// given disposable object is called after running the job constructed with
    /// the disposable object.  See also: abort, usingAsync.
    /// </summary>
    let inline using resource ([<InlineIfLambda>] x2yJ) = mkJ (Job.using resource (unwrapJ x2yJ))

    /// <summary>
    /// Implements an experimental <c>use</c> like construct for asynchronously
    /// disposable resources.  The <c>DisposeAsync</c> method of the asynchronously
    /// disposable resource is called to construct a job that is later used to
    /// dispose the resource after the constructed job returns.  See also:
    /// abort, using.
    /// </summary>
    let inline usingAsync resource ([<InlineIfLambda>] x2yJ) =
        mkJ (Job.usingAsync resource (unwrapJ x2yJ))

    /// <summary>
    /// <c>useIn x2yJ x</c> is equivalent to <c>using x x2yJ</c> and can be more convenient
    /// to use in pipelines (i.e. <c>x |&gt; useIn x2yJ</c>).
    /// </summary>
    let inline useIn ([<InlineIfLambda>] x2yJ) resource = mkJ (Job.useIn (unwrapJ x2yJ) resource)

    /// <summary>
    /// Creates a job that runs the given job sequentially the given number of
    /// times.
    /// </summary>
    let inline forN n uJ = mkJ (Job.forN n (toHopacJob uJ))

    /// <summary>
    /// <c>forNIgnore n xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; forN n</c>.
    /// </summary>
    let inline forNIgnore n xJ = mkJ (Job.forNIgnore n (toHopacJob xJ))

    /// <summary>
    /// <c>forUpTo lo hi i2uJ</c> creates a job that sequentially iterates from <c>lo</c> to
    /// <c>hi</c> (inclusive) and calls the given function to construct jobs that
    /// will be executed.
    /// </summary>
    let inline forUpTo lo hi ([<InlineIfLambda>] i2uJ) = mkJ (Job.forUpTo lo hi (unwrapJ i2uJ))

    /// <summary>
    /// <c>forUpToIgnore lo hi i2xJ</c> is equivalent to <c>forUpTo lo hi (i2xJ &gt;&gt;
    /// Job.Ignore)</c>.
    /// </summary>
    let inline forUpToIgnore lo hi ([<InlineIfLambda>] i2xJ) =
        mkJ (Job.forUpToIgnore lo hi (unwrapJ i2xJ))

    /// <summary>
    /// <c>forDownTo hi lo i2uJ</c> creates a job that sequentially iterates from <c>hi</c>
    /// to <c>lo</c> (inclusive) and calls the given function to construct jobs that
    /// will be executed.
    /// </summary>
    let inline forDownTo hi lo ([<InlineIfLambda>] i2uJ) =
        mkJ (Job.forDownTo hi lo (unwrapJ i2uJ))

    /// <summary>
    /// <c>forDownToIgnore hi lo i2xJ</c> is equivalent to <c>forDownTo hi lo (i2xJ &gt;&gt;
    /// Job.Ignore)</c>.
    /// </summary>
    let inline forDownToIgnore hi lo ([<InlineIfLambda>] i2xJ) =
        mkJ (Job.forDownToIgnore hi lo (unwrapJ i2xJ))

    /// <summary>
    /// <c>whileDo u2b uJ</c> creates a job that sequentially executes the <c>uJ</c> job as
    /// long as <c>u2b ()</c> returns <c>true</c>.  See also: whileDoDelay.
    /// </summary>
    let inline whileDo ([<InlineIfLambda>] u2b) uJ = mkJ (Job.whileDo u2b (toHopacJob uJ))

    /// <summary>
    /// <c>whileDoDelay u2b u2xJ</c> creates a job that sequentially constructs a job
    /// with <c>u2xJ</c> and executes it as long as <c>u2b ()</c> returns <c>true</c>.
    /// </summary>
    let inline whileDoDelay ([<InlineIfLambda>] u2b) ([<InlineIfLambda>] u2xJ) =
        mkJ (Job.whileDoDelay u2b (unwrapJ u2xJ))

    /// <summary>
    /// <c>whileDoIgnore u2b xJ</c> creates a job that sequentially executes the <c>xJ</c>
    /// job as long as <c>u2b ()</c> returns <c>true</c>.  <c>whileDoIgnore u2b xJ</c> is
    /// equivalent to <c>Job.Ignore xJ |&gt; whileDo u2b</c>.
    /// </summary>
    let inline whileDoIgnore ([<InlineIfLambda>] u2b) xJ = mkJ (Job.whileDoIgnore u2b (toHopacJob xJ))

    /// <summary>
    /// <c>whenDo b uJ</c> is equivalent to <c>if b then uJ else Job.unit ()</c>.
    /// </summary>
    let inline whenDo b uJ = mkJ (Job.whenDo b (toHopacJob uJ))

    /// <summary>
    /// Creates a job that repeats the given job indefinitely.  See also:
    /// foreverServer, iterate.
    /// </summary>
    let inline forever uJ = mkJ (Job.forever (toHopacJob uJ))

    /// <summary>
    /// <c>foreverIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; forever</c>.
    /// </summary>
    let inline foreverIgnore xJ = mkJ (Job.foreverIgnore (toHopacJob xJ))

    /// <summary>
    /// Creates a job that indefinitely iterates the given job constructor
    /// starting with the given value.  See also: iterateServer, forever.
    /// </summary>
    let inline iterate x ([<InlineIfLambda>] x2xJ) = mkJ (Job.iterate x (unwrapJ x2xJ))

    /// <summary>
    /// Creates a job that starts a separate server job that repeats the given job
    /// indefinitely.  <c>foreverServer xJ</c> is equivalent to <c>forever xJ |&gt; server</c>.
    /// </summary>
    let inline foreverServer uJ = mkJ (Job.foreverServer (toHopacJob uJ))

    /// <summary>
    /// Creates a job that starts a separate server job that indefinitely iterates
    /// the given job constructor starting with the given value.  <c>iterateServer x
    /// x2xJ</c> is equivalent to <c>iterate x x2xJ |&gt; server</c>.
    /// </summary>
    let inline iterateServer x ([<InlineIfLambda>] x2xJ) =
        mkJ (Job.iterateServer x (unwrapJ x2xJ))

    /// <summary>
    /// Creates a job that runs all of the jobs in sequence and returns a list of
    /// the results.  See also: seqIgnore, conCollect, Seq.mapJob.
    /// </summary>
    let inline seqCollect xJs = mkJ (Job.seqCollect (jobsOf xJs))

    /// <summary>
    /// Creates a job that runs all of the jobs as separate concurrent jobs and
    /// returns a list of the results.  See also: conIgnore, seqCollect,
    /// Seq.Con.mapJob.
    /// Note that when multiple jobs raise exceptions, then the created job raises
    /// an AggregateException.
    /// Note that this is not optimal for fine-grained parallel execution.
    /// </summary>
    let inline conCollect xJs = mkJ (Job.conCollect (jobsOf xJs))

    /// <summary>
    /// Creates a job that runs all of the jobs in sequence.  The results of the
    /// jobs are ignored.  See also: seqCollect, conIgnore, Seq.iterJob.
    /// </summary>
    let inline seqIgnore xJs = mkJ (Job.seqIgnore (jobsOf xJs))

    /// <summary>
    /// Creates a job that runs all of the jobs as separate concurrent jobs and
    /// then waits for all of the jobs to finish.  The results of the jobs are
    /// ignored.  See also: conCollect, seqIgnore, Seq.Con.iterJob.
    /// Note that when multiple jobs raise exceptions, then the created job raises
    /// an AggregateException.
    /// Note that this is not optimal for fine-grained parallel execution.
    /// </summary>
    let inline conIgnore xJs = mkJ (Job.conIgnore (jobsOf xJs))

    /// <summary>
    /// Creates a job that performs the asynchronous operation defined by the
    /// given pair of <c>doBegin</c> and <c>doEnd</c> operations.  See also:
    /// Alt.fromBeginEnd.
    /// </summary>
    let inline fromBeginEnd doBegin doEnd = mkJ (Job.fromBeginEnd doBegin doEnd)

    /// <summary>
    /// <c>fromEndBegin doEnd doBegin</c> is equivalent to <c>fromBeginEnd doBegin
    /// doEnd</c>.
    /// </summary>
    let inline fromEndBegin doEnd doBegin = mkJ (Job.fromEndBegin doEnd doBegin)

    /// <summary>
    /// Creates a job that starts an asynchronous operation by calling the given
    /// function with success and failure continuations of which exactly one must
    /// be called once.
    /// </summary>
    let inline fromContinuations kont = mkJ (Job.fromContinuations kont)

    /// <summary>
    /// Creates a job that queues the given thunk to execute on the system
    /// ThreadPool and then waits for the result of the thunk.
    /// </summary>
    let inline onThreadPool ([<InlineIfLambda>] u2x) = mkJ (Job.onThreadPool u2x)

    /// <summary>
    /// Creates a job that starts the given async operation and waits for it to
    /// complete.  See also: Alt.fromAsync.
    /// </summary>
    let inline fromAsync xA = mkJ (Job.fromAsync xA)

    /// <summary>
    /// Creates an async operation that starts the given job and waits for it to
    /// complete.
    /// </summary>
    let inline toAsync x = Job.toAsync (toHopacJob x)

    /// <summary>
    /// <c>bindAsync x2yJ xA</c> is equivalent to <c>fromAsync xA &gt;&gt;= x2yJ</c>.
    /// </summary>
    let inline bindAsync ([<InlineIfLambda>] x2yJ) xA = mkJ (Job.bindAsync (unwrapJ x2yJ) xA)

    /// <summary>
    /// Creates a job that calls the given function to start a task and waits for
    /// it to complete.  See also: Alt.fromTask.
    /// </summary>
    let inline fromTask ([<InlineIfLambda>] u2xT) = mkJ (Job.fromTask u2xT)

    /// <summary>
    /// Creates a job that calls the given function to start a task and waits for
    /// it to complete.  See also: Alt.fromUnitTask.
    /// </summary>
    let inline fromUnitTask ([<InlineIfLambda>] u2uT) = mkJ (Job.fromUnitTask u2uT)

    /// <summary>
    /// <c>liftTask x2yT</c> is equivalent to <c>fun x -&gt; fromTask &lt;| fun () -&gt; x2yT x</c>.
    /// </summary>
    let inline liftTask ([<InlineIfLambda>] x2yT) x = mkJ (Job.liftTask x2yT x)

    /// <summary>
    /// <c>liftUnitTask x2uT</c> is equivalent to <c>fun x -&gt; fromUnitTask &lt;| fun () -&gt;
    /// x2uT x</c>.
    /// </summary>
    let inline liftUnitTask ([<InlineIfLambda>] x2uT) x = mkJ (Job.liftUnitTask x2uT x)

    /// <summary>
    /// Creates a job that waits for the given task to finish and then returns the
    /// result of the task.  Note that this does not start the task.  Make sure
    /// that the task is started correctly.  Exceptions thrown during task
    /// initialization may not be caught. Prefer fromTask or liftTask.
    /// </summary>
    let inline awaitTask (xT: Task<_>) = mkJ (Job.awaitTask xT)

    /// <summary>
    /// Creates a job that waits until the given task finishes.  Note that this
    /// does not start the task.  Make sure that the task is started correctly.
    /// Exceptions thrown during task initialization may not be caught. Prefer
    /// fromUnitTask or liftUnitTask.
    /// </summary>
    let inline awaitUnitTask (uT: Task) = mkJ (Job.awaitUnitTask uT)

    /// <summary>
    /// <c>bindTask x2yJ xT</c> is equivalent to <c>awaitTask xT &gt;&gt;= x2yJ</c>.
    /// Exceptions thrown during task initialization may not be caught. Prefer
    /// fromTask or liftTask to convert the task to a Job and use Job.bind.
    /// </summary>
    let inline bindTask ([<InlineIfLambda>] x2yJ) xT = mkJ (Job.bindTask (unwrapJ x2yJ) xT)

    /// <summary>
    /// <c>bindUnitTask u2xJ uT</c> is equivalent to <c>awaitUnitTask uT &gt;&gt;= u2xJ</c>.
    /// Exceptions thrown during task initialization may not be caught. Prefer
    /// fromUnitTask or liftUnitTask to convert the task to a Job and
    /// use Job.bind.
    /// </summary>
    let inline bindUnitTask ([<InlineIfLambda>] u2xJ) uT = mkJ (Job.bindUnitTask (unwrapJ u2xJ) uT)

    /// <summary>
    /// Creates a job that immediately starts running the given job as a separate
    /// concurrent job.  Use Promise.start if you need to be able to get the
    /// result.  Use Job.server if the job never returns normally.  See also:
    /// Job.queue, Proc.start.
    /// </summary>
    let inline start uJ = mkJ (Job.start (toHopacJob uJ))

    /// <summary>
    /// Creates a job that immediately starts running the given job as a separate
    /// concurrent job.  <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt;
    /// start</c>.
    /// </summary>
    let inline startIgnore xJ = mkJ (Job.startIgnore (toHopacJob xJ))

    /// <summary>
    /// Creates a job that schedules the given job to be run as a separate
    /// concurrent job.  Use Promise.queue if you need to be able to get the
    /// result.  See also: Proc.queue.
    /// </summary>
    let inline queue uJ = mkJ (Job.queue (toHopacJob uJ))

    /// <summary>
    /// Creates a job that schedules the given job to be run as a separate
    /// concurrent job.  <c>queueIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt;
    /// queue</c>.
    /// </summary>
    let inline queueIgnore xJ = mkJ (Job.queueIgnore (toHopacJob xJ))

    /// <summary>
    /// Creates a job that immediately starts running the given job as a separate
    /// concurrent job like start, but the given job is known never to return
    /// normally, so the job can be spawned in an even more lightweight manner.
    /// </summary>
    let inline server xJ = mkJ (Job.server (toHopacJob xJ))

    /// <summary>
    /// Given a job, creates a new job that behaves exactly like the given job,
    /// except that the new job obviously cannot be directly downcast to the
    /// underlying type of the given job.  This operation is provided for
    /// debugging purposes.  You can always break abstractions using reflection.
    /// See also: Alt.paranoid.
    /// </summary>
    let inline paranoid x = mkJ (Job.paranoid (toHopacJob x))

    /// <summary>
    /// Operations on the built-in pseudo random number generator (PRNG) of Hopac.
    /// </summary>
    module Random =
        /// <summary>
        /// Returns a job that generates a pseudo random 64-bit unsigned integer.
        /// <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline get () = mkJ (Job.Random.get ())

        /// <summary>
        /// <c>map r2x</c> is equivalent to <c>bind (r2x &gt;&gt; result)</c>.
        /// </summary>
        let inline map ([<InlineIfLambda>] r2x) = mkJ (Job.Random.map r2x)

        /// <summary>
        /// <c>bind r2xJ</c> creates a job that calls the given job constructor with a
        /// pseudo random 64-bit unsigned integer.
        /// </summary>
        let inline bind ([<InlineIfLambda>] r2xJ) = mkJ (Job.Random.bind (unwrapJ r2xJ))

    /// <summary>Operations for dealing with the scheduler.</summary>
    module Scheduler =
        /// <summary>
        /// <c>bind s2xJ</c> creates a job that calls the given job constructor with the
        /// scheduler under which the job is being executed.  bind allows
        /// interfacing Hopac with existing asynchronous operations that do not fall
        /// into a pattern that is already supported explicitly.
        /// </summary>
        let inline bind ([<InlineIfLambda>] s2xJ) = mkJ (Job.Scheduler.bind (unwrapJ s2xJ))

        /// <summary>
        /// Returns a job that returns the scheduler under which the job is being
        /// run.  <c>get ()</c> is equivalent to <c>bind result</c>.
        /// </summary>
        let inline get () = mkJ (Job.Scheduler.get ())

        /// <summary>
        /// Returns a job that ensures that the immediately following operation will
        /// be executed on a Hopac worker thread.
        /// </summary>
        let inline switchToWorker () = mkJ (Job.Scheduler.switchToWorker ())

        /// <summary>
        /// <c>isolate u2x</c> is like <c>thunk u2x</c>, but it is ensured that the blocking
        /// invocation of <c>u2x</c> does not prevent scheduling of other work.
        /// </summary>
        let inline isolate ([<InlineIfLambda>] u2x) = mkJ (Job.Scheduler.isolate u2x)

    /// <summary>Operations on the global scheduler.</summary>
    module Global =
        /// <summary>
        /// Starts running the given job on the global scheduler and then blocks the
        /// current thread waiting for the job to either return successfully or
        /// fail.
        /// </summary>
        let inline run x = Hopac.run (toHopacJob x)

        /// <summary>
        /// Starts running the given job on the global scheduler, but does not wait
        /// for the job to finish.  See also: queue, server.
        /// </summary>
        let inline start uJ = Hopac.start (toHopacJob uJ)

        /// <summary>
        /// Starts running the given job on the global scheduler, but does not wait
        /// for the job to finish.  <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ
        /// |&gt; start</c>.
        /// </summary>
        let inline startIgnore xJ = Hopac.startIgnore (toHopacJob xJ)

        /// <summary>
        /// Starts running the given job on the global scheduler, but does not wait
        /// for the job to finish.  Upon the failure or success of the job, one of
        /// the given actions is called once.
        /// </summary>
        let inline startWithActions e2u x2u x = startWithActions e2u x2u (toHopacJob x)

        /// <summary>
        /// Queues the job for execution on the global scheduler.  See also:
        /// start, server.
        /// </summary>
        let inline queue uJ = Hopac.queue (toHopacJob uJ)

        /// <summary>
        /// Queues the job for execution on the global scheduler.  <c>queueIgnore xJ</c>
        /// is equivalent to <c>Job.Ignore xJ |&gt; queue</c>.
        /// </summary>
        let inline queueIgnore xJ = Hopac.queueIgnore (toHopacJob xJ)

        /// <summary>
        /// Like Job.Global.start, but the given job is known never to return
        /// normally, so the job can be spawned in an even more lightweight manner.
        /// </summary>
        let inline server xJ = Hopac.server (toHopacJob xJ)

/// <summary>
/// Operations on first-class synchronous operations or alternatives.
/// </summary>
module AltM =
    open System.Threading
    open System.Threading.Tasks

    /// <summary>Wrap a Hopac Alt.</summary>
    let inline ofAlt x = mkA x

    /// <summary>Unwrap to a Hopac Alt.</summary>
    let inline toAlt x = unA x

    /// <summary>
    /// Creates an alternative that is always available and results in the given
    /// value.
    /// Note that when there are alternatives immediately available in a choice,
    /// the first such alternative will be committed to.
    /// </summary>
    let inline always x = mkA (Alt.always x)

    /// <summary>
    /// Returns an alternative that is always available and results in the unit
    /// value.  <c>unit ()</c> is an optimized version of <c>always ()</c>.
    /// </summary>
    let inline unit () = mkA (Alt.unit ())

    /// <summary>
    /// Returns an alternative that can be committed to once and that produces the
    /// given value.
    /// </summary>
    let inline once x = mkA (Alt.once x)

    /// <summary>
    /// Creates an alternative that is never available.
    /// Note that synchronizing on <c>never ()</c>, without other alternatives, is
    /// equivalent to performing <c>abort ()</c>.
    /// </summary>
    let inline never () = mkA (Alt.never ())

    /// <summary>
    /// Returns an alternative that is never available.  <c>zero ()</c> is an optimized
    /// version of <c>never ()</c>.
    /// </summary>
    let inline zero () = mkA (Alt.zero ())

    /// <summary>
    /// Creates an alternative that has the effect of raising the specified
    /// exception.  <c>raises e</c> is equivalent to <c>prepareFun &lt;| fun () -&gt; raise e</c>.
    /// </summary>
    let inline raises e = mkA (Alt.raises e)

    /// <summary>
    /// <c>Ignore xA</c> is equivalent to <c>xA ^-&gt; always ()</c>.
    /// </summary>
    let inline Ignore x = mkA (Alt.Ignore (unA x))

    /// <summary>
    /// Creates an alternative which is committed to when the given alternative
    /// is committed to. Once committed, the given alternative's result is mapped
    /// using the given function, providing the final result.
    /// <c>xA |&gt; afterFun x2y</c> is equivalent to <c>xA |&gt; afterJob (x2y &gt;&gt; result)</c>.
    /// This is the same as <c>^-&gt;</c> with the arguments flipped.
    /// </summary>
    let inline afterFun ([<InlineIfLambda>] x2y) x = mkA (Alt.afterFun x2y (unA x))

    /// <summary>
    /// Creates an alternative whose result is passed to the given job constructor
    /// and processed with the resulting job after the given alternative has been
    /// committed to.  This is the same as <c>^=&gt;</c> with the arguments flipped.
    /// </summary>
    let inline afterJob ([<InlineIfLambda>] x2yJ) x = mkA (Alt.afterJob (unwrapJ x2yJ) (unA x))

    /// <summary>
    /// Creates an alternative that is available when any one of the given
    /// alternatives is.  See also: choosy, <c>&lt;|&gt;</c>.
    /// Note that <c>choose []</c> is equivalent to <c>never ()</c>.
    /// </summary>
    let inline choose xAs = mkA (Alt.choose (altsOf xAs))

    /// <summary>
    /// <c>choosy xAs</c> (read: choose array) is an optimized version of <c>choose xAs</c>
    /// when <c>xAs</c> is an array.  Do not write <c>choosy (Seq.toArray xAs)</c> instead
    /// of <c>choose xAs</c> unless the resulting alternative is reused many times.
    /// </summary>
    let inline choosy (xAs: AltM<'T>[]) =
        let ys = Array.zeroCreate xAs.Length

        for i = 0 to xAs.Length - 1 do
            ys[i] <- unA xAs[i]

        mkA (Alt.choosy ys)

    /// <summary>
    /// <c>chooser xAs</c> is like <c>choose xAs</c> except that the order in which the
    /// alternatives from the sequence are instantiated will be determined at
    /// random each time the alternative is used.  See also: <c>&lt;~&gt;</c>.
    /// </summary>
    let inline chooser xAs = mkA (Alt.chooser (altsOf xAs))

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// the given function, which will be called with a pseudo random 64-bit
    /// unsigned integer.  See also: Random.bind.
    /// </summary>
    let inline random ([<InlineIfLambda>] r2xA) = mkA (Alt.random (unwrapA r2xA))

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given thunk.  See also: <c>*&lt;-=&gt;-</c>, prepareJob.
    /// </summary>
    let inline prepareFun ([<InlineIfLambda>] u2xA) = mkA (Alt.prepareFun (unwrapA u2xA))

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given job.  See also: <c>*&lt;-=&gt;-</c>, prepareFun, withNackJob.
    /// </summary>
    let inline prepareJob ([<InlineIfLambda>] u2xAJ) =
        mkA (
            Alt.prepareJob (fun () ->
                let (JobM j) = u2xAJ ()
                Job.map unA j)
        )

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given job.  <c>prepare xAJ</c> is equivalent to <c>prepareJob &lt;| fun () -&gt; xAJ</c>.
    /// </summary>
    let inline prepare xAJ = mkA (Alt.prepare (Job.map unA (toHopacJob xAJ)))

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given job constructed with a negative acknowledgment alternative.  See
    /// also: <c>*&lt;+-&gt;-</c>, withNackFun, prepareJob.
    /// </summary>
    let inline withNackJob ([<InlineIfLambda>] n2xAJ) =
        mkA (
            Alt.withNackJob (fun nack ->
                let (JobM j) = n2xAJ nack
                Job.map unA j)
        )

    /// <summary>
    /// <c>withNackFun n2xA</c> is equivalent to <c>withNackJob (Job.lift n2xA)</c>.
    /// </summary>
    let inline withNackFun ([<InlineIfLambda>] n2xA) = mkA (Alt.withNackFun (unwrapA n2xA))

    /// <summary>
    /// Returns a new alternative that that makes it so that the given job will be
    /// started as a separate concurrent job if the given alternative isn't the
    /// one being committed to.  See also: wrapAbortFun, withNackJob.
    /// </summary>
    let inline wrapAbortJob uJ x = mkA (Alt.wrapAbortJob (toHopacJob uJ) (unA x))

    /// <summary>
    /// <c>wrapAbortFun u2u xA</c> is equivalent to <c>wrapAbortJob (Job.thunk u2u) xA</c>.
    /// </summary>
    let inline wrapAbortFun ([<InlineIfLambda>] u2u) x = mkA (Alt.wrapAbortFun u2u (unA x))

    /// <summary>
    /// Implements the <c>try-in-unless</c> exception handling construct for
    /// alternatives.  Both of the continuation jobs <c>'x -&gt; Job&lt;'y&gt;</c>, for success,
    /// and <c>exn -&gt; Job&lt;'y&gt;</c>, for failure, are invoked from a tail position.
    /// Exceptions from both before and after the commit point can be handled.  An
    /// exception that occurs before a commit point, from the user code in a
    /// prepareJob, or withNackJob, results in treating that exception as the
    /// commit point.
    /// Note you can also use function or job level exception handling before the
    /// commit point within the user code in a prepareJob or withNackJob.
    /// </summary>
    let inline tryIn x ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] e2yJ) =
        mkA (Alt.tryIn (unA x) (unwrapJ x2yJ) (unwrapJ e2yJ))

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for alternatives.  The given action, specified as a function, is executed
    /// after the alternative has been committed to, whether the alternative fails
    /// or completes successfully.  Note that the action is not executed in case
    /// the alternative is not committed to.  Use withNackJob to attach the
    /// action to the non-committed case.
    /// </summary>
    let inline tryFinallyFun x ([<InlineIfLambda>] u2u) = mkA (Alt.tryFinallyFun (unA x) u2u)

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for alternatives.  The given action, specified as a job, is executed after
    /// the alternative has been committed to, whether the alternative fails or
    /// completes successfully.  Note that the action is not executed in case the
    /// alternative is not committed to.  Use withNackJob to attach the action
    /// to the non-committed case.
    /// </summary>
    let inline tryFinallyJob x uJ = mkA (Alt.tryFinallyJob (unA x) (toHopacJob uJ))

    /// <summary>
    /// Creates an alternative that, when instantiated, starts the cancellable
    /// asynchronous operation defined by the given <c>doBegin</c>, <c>doEnd</c> and
    /// <c>doCancel</c> operations and waits for it to complete, after which the
    /// alternative becomes available.  If some other alternative is committed to
    /// a in a choice before the operation completes, then the operation is
    /// cancelled. See also: Job.fromBeginEnd.
    /// </summary>
    let inline fromBeginEnd doBegin doEnd doCancel = mkA (Alt.fromBeginEnd doBegin doEnd doCancel)

    /// <summary>
    /// Creates an alternative that, when instantiated, starts the given
    /// cancellable async operation and waits for it to complete, after which the
    /// alternative becomes available.  If some other alternative is committed to
    /// in a choice before the operation completes, then the operation is
    /// cancelled.  See also: Job.fromAsync.
    /// </summary>
    let inline fromAsync xA = mkA (Alt.fromAsync xA)

    /// <summary>
    /// Creates an async operation that starts the given alternative and waits for
    /// it to be committed to.  If the async operation is cancelled before the
    /// alternative is committed to, an attempt is made to also cancel the
    /// alternative by making a cancellation alternative available.  Note that
    /// cancellation is not transactional and <c>Alt.toAsync &gt;&gt; Alt.fromAsync</c> is
    /// not the identity function.  See also: Job.toAsync.
    /// </summary>
    let inline toAsync x = Alt.toAsync (unA x)

    /// <summary>
    /// Creates an alternative that, when instantiated, calls the given function
    /// with a cancellation token to start a cancellable task and waits for it to
    /// complete, after which the alternative becomes available.  If some other
    /// alternative is committed to in a choice before the task completes, then
    /// the token will be cancelled.  See also: Job.fromTask.
    /// </summary>
    let inline fromTask ([<InlineIfLambda>] u2xT: CancellationToken -> Task<_>) = mkA (Alt.fromTask u2xT)

    /// <summary>
    /// Creates an alternative that, when instantiated, calls the given function
    /// with a cancellation token to start a cancellable task and waits for it to
    /// complete, after which the alternative becomes available.  If some other
    /// alternative is committed to in a choice before the task completes, then
    /// the token will be cancelled.  See also: Job.fromUnitTask.
    /// </summary>
    let inline fromUnitTask ([<InlineIfLambda>] u2uT: CancellationToken -> Task) = mkA (Alt.fromUnitTask u2uT)

    /// <summary>
    /// Given an alternative, creates a new alternative that behaves exactly like
    /// the given alternative, except that the new alternative obviously cannot be
    /// directly downcast to the underlying type of the given alternative.  This
    /// operation is provided for debugging purposes.  You can always break
    /// abstractions using reflection.  See also: Job.paranoid.
    /// </summary>
    let inline paranoid x = mkA (Alt.paranoid (unA x))

    /// <summary>
    /// Creates an alternative that, after instantiation, becomes available after
    /// the specified time span.
    /// </summary>
    let inline timeOut ts = mkA (timeOut ts)

    /// <summary>
    /// <c>timeOutMillis n</c> is equivalent to <c>timeOut &lt;&lt; TimeSpan.FromMilliseconds
    /// &lt;| float n</c>.
    /// </summary>
    let inline timeOutMillis n = mkA (timeOutMillis n)

    /// <summary>
    /// Creates an alternative that yields the thread of execution to any ready
    /// work and then becomes available.
    /// </summary>
    let idle = mkA Hopac.idle

/// <summary>Expression builder type for jobs.</summary>
type JobMBuilder() =
    member inline _.Bind(x: JobM<'x>, [<InlineIfLambda>] f: 'x -> JobM<'y>) : JobM<'y> =
        mkJ (Job.bind (fun a -> unJ (f a)) (unJ x))

    member inline _.Bind(x: AltM<'x>, [<InlineIfLambda>] f: 'x -> JobM<'y>) : JobM<'y> =
        mkJ (Job.bind (fun a -> unJ (f a)) (unA x :> Job<_>))

    member inline _.Bind(x: Job<'x>, [<InlineIfLambda>] f: 'x -> JobM<'y>) : JobM<'y> =
        mkJ (Job.bind (fun a -> unJ (f a)) x)

    member inline _.Bind(x: Async<'x>, [<InlineIfLambda>] f: 'x -> JobM<'y>) : JobM<'y> = JobM.bindAsync f x

    member inline _.Bind(x: Threading.Tasks.Task<'x>, [<InlineIfLambda>] f: 'x -> JobM<'y>) : JobM<'y> =
        JobM.bindTask f x

    member inline _.Bind(x: Threading.Tasks.Task, [<InlineIfLambda>] f: unit -> JobM<'y>) : JobM<'y> =
        JobM.bindUnitTask f x

    member inline _.Bind(x: IObservable<'x>, [<InlineIfLambda>] f: 'x -> JobM<'y>) : JobM<'y> =
        JobM.ofJob (
            job {
                let! v = x
                return! JobM.toJob (f v)
            }
        )

    member inline _.Combine(a: JobM<unit>, [<InlineIfLambda>] b: unit -> JobM<'x>) : JobM<'x> =
        JobM.bind (fun () -> b ()) a

    member inline _.Combine(a: AltM<unit>, [<InlineIfLambda>] b: unit -> JobM<'x>) : JobM<'x> =
        JobM.bind (fun () -> b ()) a

    member inline _.Delay([<InlineIfLambda>] u2xJ: unit -> JobM<'x>) = u2xJ

    member inline _.For(xs: seq<'x>, [<InlineIfLambda>] body: 'x -> JobM<unit>) : JobM<unit> =
        JobM.ofJob (
            job {
                for x in xs do
                    do! JobM.toJob (body x)
            }
        )

    member inline _.Return(x: 'x) : JobM<'x> = JobM.result x

    member inline _.ReturnFrom(x: JobM<'x>) : JobM<'x> = x

    member inline _.ReturnFrom(x: AltM<'x>) : JobM<'x> = JobM.ofJobLike x

    member inline _.ReturnFrom(x: Job<'x>) : JobM<'x> = JobM.ofJob x

    member inline _.ReturnFrom(x: Async<'x>) : JobM<'x> = JobM.fromAsync x

    member inline _.ReturnFrom(x: Threading.Tasks.Task<'x>) : JobM<'x> = JobM.awaitTask x

    member inline _.ReturnFrom(x: Threading.Tasks.Task) : JobM<unit> = JobM.awaitUnitTask x

    member inline _.ReturnFrom(x: IObservable<'x>) : JobM<'x> = JobM.ofJob (job { return! x })

    member inline _.Run([<InlineIfLambda>] u2xJ: unit -> JobM<'x>) : JobM<'x> = JobM.delay u2xJ

    member inline _.TryFinally
        ([<InlineIfLambda>] u2xJ: unit -> JobM<'x>, [<InlineIfLambda>] compensation: unit -> unit)
        =
        JobM.tryFinallyFunDelay u2xJ compensation

    member inline _.TryWith
        ([<InlineIfLambda>] u2xJ: unit -> JobM<'x>, [<InlineIfLambda>] handler: exn -> JobM<'x>)
        =
        JobM.tryWithDelay u2xJ handler

    member inline _.Using(resource: #IDisposable, [<InlineIfLambda>] body) = JobM.using resource body

    member inline _.While([<InlineIfLambda>] guard: unit -> bool, [<InlineIfLambda>] body: unit -> JobM<unit>) =
        JobM.whileDoDelay guard body

    member inline _.Zero() : JobM<unit> = JobM.unit ()

/// <summary>Default expression builder for jobs.</summary>
let jobm = JobMBuilder()

/// <summary>
/// Lets Hopac's <c>job</c> bind <c>JobM</c> and non-generic <c>Task</c> the same way
/// <c>jobm</c> does.
/// </summary>
type JobBuilder with
    member inline _.Bind(x: JobM<'x>, [<InlineIfLambda>] f: 'x -> Job<'y>) : Job<'y> =
        Job.bind f (unJ x)

    member inline _.Bind(x: AltM<'x>, [<InlineIfLambda>] f: 'x -> Job<'y>) : Job<'y> =
        Job.bind f (unA x :> Job<_>)

    member inline _.Bind(x: Threading.Tasks.Task, [<InlineIfLambda>] f: unit -> Job<'y>) : Job<'y> =
        Job.bindUnitTask f x

    member inline _.ReturnFrom(x: JobM<'x>) : Job<'x> = unJ x

    member inline _.ReturnFrom(x: AltM<'x>) : Job<'x> = unA x :> Job<_>

    member inline _.ReturnFrom(x: Threading.Tasks.Task) : Job<unit> = Job.awaitUnitTask x

// ---------------------------------------------------------------------------
// 3. Top-level Hopac module, taking JobM / AltM
// These shadow Hopac.run / start / queue for Job. Use Hopac.run on a bare Job.
// timeOut / timeOutMillis / idle stay on AltM so Hopac.timeOutMillis still
// returns Alt and works inside job { }.
// ---------------------------------------------------------------------------

/// <summary>
/// Starts running the given job and then blocks the current thread waiting
/// for the job to either return successfully or fail.  See also: start.
/// </summary>
let inline run x = run (toHopacJob x)

/// <summary>
/// <c>runDelay u2xJ</c> is equivalent to <c>run &lt;| Job.delay u2xJ</c>.
/// </summary>
let inline runDelay ([<InlineIfLambda>] u2xJ) = runDelay (unwrapJ u2xJ)

/// <summary>
/// Starts running the given job, but does not wait for the job to finish.
/// See also: queue, server.
/// </summary>
let inline start uJ = start (toHopacJob uJ)

/// <summary>
/// Starts running the given job, but does not wait for the job to finish.
/// <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; start</c>.
/// </summary>
let inline startIgnore xJ = startIgnore (toHopacJob xJ)

/// <summary>
/// Starts running the given delayed job, but does not wait for the job to
/// finish.  <c>startDelay u2xJ</c> is equivalent to <c>startIgnore &lt;| Job.delay
/// u2xJ</c>.
/// </summary>
let inline startDelay ([<InlineIfLambda>] u2xJ) = startDelay (unwrapJ u2xJ)

/// <summary>
/// Starts running the given job, but does not wait for the job to finish.
/// Upon the failure or success of the job, one of the given actions is called
/// once.
/// </summary>
let inline startWithActions e2u x2u x = startWithActions e2u x2u (toHopacJob x)

/// <summary>
/// Starts running the given job.  The result can be obtained from the
/// returned task.
/// </summary>
let inline startAsTask x = startAsTask (toHopacJob x)

/// <summary>
/// Queues the given job for execution.  See also: start, server.
/// </summary>
let inline queue uJ = queue (toHopacJob uJ)

/// <summary>
/// Queues the given job for execution.  <c>queueIgnore xJ</c> is equivalent to
/// <c>Job.Ignore xJ |&gt; queue</c>.
/// </summary>
let inline queueIgnore xJ = queueIgnore (toHopacJob xJ)

/// <summary>
/// Queues the given delayed job for execution.  <c>queueDelay u2xJ</c> is
/// equivalent to <c>queueIgnore &lt;| Job.delay u2xJ</c>.
/// </summary>
let inline queueDelay ([<InlineIfLambda>] u2xJ) = queueDelay (unwrapJ u2xJ)

/// <summary>
/// Queues the given job for execution.  The result can be obtained from the
/// returned task.
/// </summary>
let inline queueAsTask x = queueAsTask (toHopacJob x)

/// <summary>
/// Starts running the given job like start, but the given job is known
/// never to return normally, so the job can be spawned in an even more
/// lightweight manner.
/// </summary>
let inline server xJ = server (toHopacJob xJ)

/// <summary>
/// Creates a promise whose value is computed lazily with the given job when
/// an attempt is made to read the promise.
/// </summary>
let inline memo x = memo (toHopacJob x)

/// <summary>
/// Use object as job.  This function is a NOP and is provided as a kind of
/// syntactic alternative to using a type ascription or an upcast.
/// </summary>
let inline asJobM (x: JobM<'T>) = x

/// <summary>
/// Use object as alternative.  This function is a NOP and is provided as a
/// syntactic alternative to using a type ascription or an upcast.
/// </summary>
let inline asAlt (x: AltM<'T>) = x

/// <summary>
/// Hopac infix operators, lifted to <c>JobM</c> / <c>AltM</c>.  Open this module
/// instead of <c>Hopac.Infixes</c> when working with the wrapped types.
/// Message-passing operators still take Hopac <c>Ch</c> / <c>IVar</c> / <c>MVar</c> /
/// <c>Mailbox</c> on the left and wrap the resulting job or alternative.
/// Memoizing <c>*</c>-suffixed operators return a Hopac <c>Promise</c>.
/// </summary>
module Infixes =

    // Query-Reply

    /// <summary>
    /// Creates an alternative that, using the given job constructor, constructs a
    /// query with a reply channel and a nack, sends it to the query channel and
    /// commits on taking the reply from the reply channel.  See also: <c>*&lt;+-&gt;-</c>.
    /// </summary>
    let inline ( *<+->= ) qCh ([<InlineIfLambda>] rCh2n2qJ) =
        mkA (Infixes.( *<+->= ) qCh (fun rCh nack -> toHopacJob (rCh2n2qJ rCh nack)))

    /// <summary>
    /// Creates an alternative that, using the given function, constructs a query
    /// with a reply channel and a nack, sends it to the query channel and commits
    /// on taking the reply from the reply channel.  <c>*&lt;+-&gt;-</c> captures the most
    /// common use case of <c>Alt.withNackJob</c> and is a slightly less expressive
    /// form of <c>*&lt;+-&gt;=</c>.  See also: <c>*&lt;-=&gt;-</c>.
    /// </summary>
    let inline ( *<+->- ) qCh ([<InlineIfLambda>] rCh2n2q) =
        mkA (Infixes.( *<+->- ) qCh rCh2n2q)

    /// <summary>
    /// Creates an alternative that, using the given job constructor, constructs a
    /// query with a reply variable, commits on giving the query and reads the
    /// reply variable.  See also: <c>*&lt;-=&gt;-</c>.
    /// </summary>
    let inline ( *<-=>= ) qCh ([<InlineIfLambda>] rI2qJ) =
        mkA (Infixes.( *<-=>= ) qCh (unwrapJ rI2qJ))

    /// <summary>
    /// Creates an alternative that, using the given function, constructs a query
    /// with a reply variable, commits on giving the query and reads the reply
    /// variable.  <c>*&lt;-=&gt;-</c> captures the most common use case of
    /// <c>Alt.prepareFun</c> and is a slightly less expressive form of <c>*&lt;-=&gt;=</c>.
    /// See also: <c>*&lt;+-&gt;-</c>.
    /// </summary>
    let inline ( *<-=>- ) qCh ([<InlineIfLambda>] rI2q) =
        mkA (Infixes.( *<-=>- ) qCh rI2q)

    /// <summary>
    /// Creates an alternative that, using the given job constructor, constructs a
    /// query with a reply variable, sends the query and reads the reply.  In
    /// order for the alternative to make sense, the operation must not require
    /// exclusive choice.  If this is not the case, then the resulting value
    /// should only be used as a job.
    /// </summary>
    let inline ( *<+=>= ) qCh ([<InlineIfLambda>] rI2qJ) =
        mkA (Infixes.( *<+=>= ) qCh (unwrapJ rI2qJ))

    /// <summary>
    /// Creates an alternative that, using the given function, constructs a query
    /// with a reply variable, sends the query and reads the reply.  In order for
    /// the alternative to make sense, the operation must not require exclusive
    /// choice.  If this is not the case, then the resulting value should only be
    /// used as a job.
    /// </summary>
    let inline ( *<+=>- ) qCh ([<InlineIfLambda>] rI2q) =
        mkA (Infixes.( *<+=>- ) qCh rI2q)

    // Message passing

    /// <summary>
    /// Creates an alternative that, at instantiation time, offers to give the
    /// given value on the given channel, and becomes available when another job
    /// offers to take the value.  <c>xCh *&lt;- x</c> is equivalent to <c>Ch.give xCh x</c>.
    /// </summary>
    let inline ( *<- ) xCh x = mkA (Infixes.( *<- ) xCh x)

    /// <summary>
    /// Creates a job that sends a value to another job on the given channel.  A
    /// send operation is asynchronous.  In other words, a send operation does not
    /// wait for another job to give the value to.  <c>xCh *&lt;+ x</c> is equivalent to
    /// <c>Ch.send xCh x</c>.
    /// </summary>
    let inline ( *<+ ) xCh x = mkJ (Infixes.( *<+ ) xCh x)

    /// <summary>
    /// Creates a job that writes to the given write once variable.  It is an
    /// error to write to a single <c>IVar</c> more than once.  <c>xI *&lt;= x</c> is
    /// equivalent to <c>IVar.fill xI x</c>.
    /// </summary>
    let inline ( *<= ) xI x = mkJ (Infixes.( *<= ) xI x)

    /// <summary>
    /// Creates a job that writes the given exception to the given write once
    /// variable.  It is an error to write to a single <c>IVar</c> more than once.
    /// <c>xI *&lt;=! e</c> is equivalent to <c>IVar.fillFailure xI e</c>.
    /// </summary>
    let inline ( *<=! ) xI e = mkJ (Infixes.( *<=! ) xI e)

    /// <summary>
    /// Creates a job that writes the given value to the serialized variable.  It
    /// is an error to write to a <c>MVar</c> that is full.  <c>xM *&lt;&lt;= x</c> is
    /// equivalent to <c>MVar.fill xM x</c>.
    /// </summary>
    let inline ( *<<= ) xM x = mkJ (Infixes.( *<<= ) xM x)

    /// <summary>
    /// Creates a job that sends the given value to the specified mailbox.  This
    /// operation never blocks.  <c>xMb *&lt;&lt;+ x</c> is equivalent to
    /// <c>Mailbox.send xMb x</c>.
    /// </summary>
    let inline ( *<<+ ) xMb x = mkJ (Infixes.( *<<+ ) xMb x)

    // After actions

    /// <summary>
    /// Creates an alternative whose result is passed to the given job constructor
    /// and processed with the resulting job after the given alternative has been
    /// committed to.  This is the same as <c>afterJob</c> with the arguments flipped.
    /// </summary>
    let inline ( ^=> ) xA ([<InlineIfLambda>] x2yJ) =
        mkA (Infixes.(^=>) (unA xA) (unwrapJ x2yJ))

    /// <summary>
    /// Creates an alternative which is committed to when the given alternative
    /// is committed to. Once committed, the given alternative's result is mapped
    /// using the given function, providing the final result.
    /// <c>xA ^-&gt; x2y</c> is equivalent to <c>xA ^=&gt; (x2y &gt;&gt; result)</c>.  This is the same
    /// as <c>afterFun</c> with the arguments flipped.
    /// </summary>
    let inline ( ^-> ) xA ([<InlineIfLambda>] x2y) =
        mkA (Infixes.(^->) (unA xA) x2y)

    /// <summary>
    /// Creates an alternative which is committed to when the given alternative
    /// is committed to. Once committed, the job argument is executed and
    /// generates the result.
    /// <c>xA ^=&gt;. yJ</c> is equivalent to <c>xA ^=&gt; always yJ</c>.
    /// </summary>
    let inline ( ^=>. ) xA yJ =
        mkA (Infixes.(^=>.) (unA xA) (toHopacJob yJ))

    /// <summary>
    /// Creates an alternative which is committed to when the given alternative
    /// is committed to. Once committed, the given value is used as the result.
    /// <c>xA ^-&gt;. y</c> is equivalent to <c>xA ^-&gt; always y</c>.
    /// </summary>
    let inline ( ^->. ) xA y = mkA (Infixes.(^->.) (unA xA) y)

    /// <summary>
    /// Creates an alternative which is committed to when the alternative
    /// argument is committed to. Once committed, the given exception is raised.
    /// <c>xA ^-&gt;! e</c> is equivalent to <c>xA ^-&gt; fun _ -&gt; raise e</c>.
    /// </summary>
    let inline ( ^->! ) xA e = mkA (Infixes.(^->!) (unA xA) e)

    // Choices

    /// <summary>
    /// Creates an alternative that is available when either of the given
    /// alternatives is available.  <c>xA1 &lt;|&gt; xA2</c> is an optimized version of
    /// <c>choose [xA1; xA2]</c>.  See also: choosy.
    /// </summary>
    let inline ( <|> ) xA1 xA2 =
        mkA (Infixes.(<|>) (unA xA1) (unA xA2))

    /// <summary>A memoizing version of <c>&lt;|&gt;</c>.</summary>
    let inline ( <|>* ) xA1 xA2 =
        Infixes.(<|>*) (unA xA1) (unA xA2)

    /// <summary>
    /// <c>xA1 &lt;~&gt; xA2</c> is like <c>xA1 &lt;|&gt; xA2</c> except that the order in which
    /// <c>xA1</c> and <c>xA2</c> are instantiated is determined at random every time the
    /// alternative is used.  See also: chooser.
    /// </summary>
    let inline ( <~> ) xA1 xA2 =
        mkA (Infixes.(<~>) (unA xA1) (unA xA2))

    /// <summary>A memoizing version of <c>&lt;~&gt;</c>.</summary>
    let inline ( <~>* ) xA1 xA2 =
        Infixes.(<~>*) (unA xA1) (unA xA2)

    // Sequencing

    /// <summary>
    /// Creates a job that first runs the given job and then passes the result of
    /// that job to the given function to build another job which will then be
    /// run.  This is the same as bind with the arguments flipped.
    /// </summary>
    let inline ( >>= ) xJ ([<InlineIfLambda>] x2yJ) =
        mkJ (Infixes.(>>=) (toHopacJob xJ) (unwrapJ x2yJ))
        
    let inline megaBind xJ ([<InlineIfLambda>] x2yJ) =
        mkJ (Infixes.(>>=) (toHopacJob xJ) (unwrapJ x2yJ))

    /// <summary>A memoizing version of <c>&gt;&gt;=</c>.</summary>
    let inline ( >>=* ) xJ ([<InlineIfLambda>] x2yJ) =
        Infixes.(>>=*) (toHopacJob xJ) (unwrapJ x2yJ)

    /// <summary>
    /// Creates a job that runs the given job and maps the result of the job with
    /// the given function.  <c>xJ &gt;&gt;- x2y</c> is an optimized version of
    /// <c>xJ &gt;&gt;= (x2y &gt;&gt; result)</c>.  This is the same as map with the arguments
    /// flipped.
    /// </summary>
    let inline ( >>- ) xJ ([<InlineIfLambda>] x2y) =
        mkJ (Infixes.(>>-) (toHopacJob xJ) x2y)

    /// <summary>A memoizing version of <c>&gt;&gt;-</c>.</summary>
    let inline ( >>-* ) xJ ([<InlineIfLambda>] x2y) =
        Infixes.(>>-*) (toHopacJob xJ) x2y

    /// <summary>
    /// Creates a job that runs the given two jobs and returns the result of the
    /// second job.  <c>xJ &gt;&gt;=. yJ</c> is equivalent to <c>xJ &gt;&gt;= always yJ</c>.
    /// </summary>
    let inline ( >>=. ) xJ yJ =
        mkJ (Infixes.(>>=.) (toHopacJob xJ) (toHopacJob yJ))

    /// <summary>A memoizing version of <c>&gt;&gt;=.</c>.</summary>
    let inline ( >>=*. ) xJ yJ =
        Infixes.(>>=*.) (toHopacJob xJ) (toHopacJob yJ)

    /// <summary>
    /// Creates a job that runs the given job and then returns the given value.
    /// <c>xJ &gt;&gt;-. y</c> is an optimized version of <c>xJ &gt;&gt;= always (result y)</c>.
    /// </summary>
    let inline ( >>-. ) xJ y =
        mkJ (Infixes.(>>-.) (toHopacJob xJ) y)

    /// <summary>A memoizing version of <c>&gt;&gt;-.</c>.</summary>
    let inline ( >>-*. ) xJ y =
        Infixes.(>>-*.) (toHopacJob xJ) y

    /// <summary>
    /// Creates a job that runs the given job and then raises the given exception.
    /// <c>xJ &gt;&gt;-! e</c> is equivalent to <c>xJ &gt;&gt;= fun _ -&gt; raise e</c>.
    /// </summary>
    let inline ( >>-! ) xJ e =
        mkJ (Infixes.(>>-!) (toHopacJob xJ) e)

    /// <summary>A memoizing version of <c>&gt;&gt;-!</c>.</summary>
    let inline ( >>-*! ) xJ e =
        Infixes.(>>-*!) (toHopacJob xJ) e

    // Composition

    /// <summary>
    /// Creates a job that is the composition of the given two job constructors.
    /// <c>(x2yJ &gt;=&gt; y2zJ) x</c> is equivalent to <c>x2yJ x &gt;&gt;= y2zJ</c> and is much like
    /// the <c>&gt;&gt;</c> operator on ordinary functions.
    /// </summary>
    let inline ( >=> ) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2zJ) x =
        mkJ (Infixes.(>=>) (unwrapJ x2yJ) (unwrapJ y2zJ) x)

    /// <summary>A memoizing version of <c>&gt;=&gt;</c>.</summary>
    let inline ( >=>* ) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2zJ) x =
        Infixes.(>=>*) (unwrapJ x2yJ) (unwrapJ y2zJ) x

    /// <summary>
    /// Creates a job that is the composition of the given job constructor and
    /// function.  <c>(x2yJ &gt;-&gt; y2z) x</c> is equivalent to <c>x2yJ x &gt;&gt;- y2z</c> and is
    /// much like the <c>&gt;&gt;</c> operator on ordinary functions.
    /// </summary>
    let inline ( >-> ) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2z) x =
        mkJ (Infixes.(>->) (unwrapJ x2yJ) y2z x)

    /// <summary>A memoizing version of <c>&gt;-&gt;</c>.</summary>
    let inline ( >->* ) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2z) x =
        Infixes.(>->*) (unwrapJ x2yJ) y2z x

    /// <summary>
    /// <c>(x2yJ &gt;=&gt;. zJ) x</c> is equivalent to <c>x2yJ x &gt;&gt;=. zJ</c>.
    /// </summary>
    let inline ( >=>. ) ([<InlineIfLambda>] x2yJ) zJ x =
        mkJ (Infixes.(>=>.) (unwrapJ x2yJ) (toHopacJob zJ) x)

    /// <summary>A memoizing version of <c>&gt;=&gt;.</c>.</summary>
    let inline ( >=>*. ) ([<InlineIfLambda>] x2yJ) zJ x =
        Infixes.(>=>*.) (unwrapJ x2yJ) (toHopacJob zJ) x

    /// <summary>
    /// <c>(x2yJ &gt;-&gt;. z) x</c> is equivalent to <c>x2yJ x &gt;&gt;-. z</c>.
    /// </summary>
    let inline ( >->. ) ([<InlineIfLambda>] x2yJ) z x =
        mkJ (Infixes.(>->.) (unwrapJ x2yJ) z x)

    /// <summary>A memoizing version of <c>&gt;-&gt;.</c>.</summary>
    let inline ( >->*. ) ([<InlineIfLambda>] x2yJ) z x =
        Infixes.(>->*.) (unwrapJ x2yJ) z x

    /// <summary>
    /// <c>(x2yJ &gt;-&gt;! e) x</c> is equivalent to <c>x2yJ x &gt;&gt;-! e</c>.
    /// </summary>
    let inline ( >->! ) ([<InlineIfLambda>] x2yJ) e x =
        mkJ (Infixes.(>->!) (unwrapJ x2yJ) e x)

    /// <summary>A memoizing version of <c>&gt;-&gt;!</c>.</summary>
    let inline ( >->*! ) ([<InlineIfLambda>] x2yJ) e x =
        Infixes.(>->*!) (unwrapJ x2yJ) e x

    // Pairing

    /// <summary>
    /// Creates a job that runs the given two jobs and then returns a pair of
    /// their results.  <c>xJ &lt;&amp;&gt; yJ</c> is equivalent to
    /// <c>xJ &gt;&gt;= fun x -&gt; yJ &gt;&gt;= fun y -&gt; result (x, y)</c>.
    /// </summary>
    let inline ( <&> ) xJ yJ =
        mkJ (Infixes.(<&>) (toHopacJob xJ) (toHopacJob yJ))

    /// <summary>
    /// Creates a job that either runs the given jobs sequentially, like
    /// <c>&lt;&amp;&gt;</c>, or as two separate parallel jobs and returns a pair of their
    /// results.  This is Hopac's pairing operator, not FSharpPlus applicative
    /// apply.
    /// </summary>
    let inline ( <*> ) xJ yJ =
        mkJ (Infixes.(<*>) (toHopacJob xJ) (toHopacJob yJ))

    /// <summary>
    /// An alternative that is equivalent to first committing to either one of the
    /// given alternatives and then committing to the other alternative.  Note
    /// that this is not the same as committing to both of the alternatives in a
    /// single transaction.
    /// </summary>
    let inline ( <+> ) xA yA =
        mkA (Infixes.(<+>) (unA xA) (unA yA))

module private CompileCheck =
    open FSharpPlus

    let mapped: JobM<int> = map ((+) 1) (JobM.result 1)
    let bound: JobM<int> = JobM.result 1 >>= fun x -> JobM.result (x + 1)
    let applied: JobM<int> = JobM.result ((+) 1) <*> JobM.result 40
    let delayed: JobM<int> = JobM.delay (fun () -> JobM.result 1)
    let ignored: JobM<unit> = JobM.Ignore (JobM.result 1)
    let ran: int = run (JobM.result 1)

    let computed: JobM<int> =
        monad {
            let! x = JobM.result 1
            return x + 1
        }

    let fromJobm: JobM<int> =
        jobm {
            let! x = JobM.result 1
            let! y = Job.result 2
            return x + y
        }

    let fromTask: JobM<int> =
        jobm {
            let! x = Threading.Tasks.Task.FromResult 1
            do! Threading.Tasks.Task.CompletedTask
            return! Threading.Tasks.Task.FromResult(x + 1)
        }

    let monadFromTask: JobM<int> =
        monad {
            let! x = Threading.Tasks.Task.FromResult 1
            do! Threading.Tasks.Task.CompletedTask
            return x + 1
        }

    let jobBindsJobM: Job<int> =
        job {
            let! x = JobM.result 1
            do! Threading.Tasks.Task.CompletedTask
            return x
        }

    let altMapped: AltM<int> = map ((+) 1) (AltM.always 1)
    let altEmpty: AltM<int> = empty
    let altChoice: AltM<int> = AltM.always 1 <|> AltM.never ()

    let altUsedAsJob: int = run (AltM.always 1)

    let bindAltAsJob: JobM<int> =
        JobM.bind (fun x -> JobM.result (x + 1)) (AltM.always 1)

    let jobmBindsAlt: JobM<int> =
        jobm {
            let! x = AltM.always 1
            return x + 1
        }

    let jobBindsAlt: Job<int> =
        job {
            let! x = AltM.always 1
            return x + 1
        }

    let ignoreAlt: JobM<unit> = JobM.Ignore (AltM.always ())
    let mapAltAsJob: JobM<int> = JobM.map ((+) 1) (AltM.always 1)
    let altAfter: AltM<int> = AltM.afterJob (fun x -> JobM.result (x + 1)) (AltM.always 1)
    let timed: AltM<unit> = AltM.timeOutMillis 0

module private InfixCompileCheck =
    open Infixes

    let seqBind: JobM<int> = JobM.result 1 >>= fun x -> JobM.result (x + 1)
    let seqBindAlt: JobM<int> = AltM.always 1 >>= fun x -> JobM.result (x + 1)
    let seqMap: JobM<int> = JobM.result 1 >>- ((+) 1)
    let seqThen: JobM<int> = JobM.result () >>=. JobM.result 2
    let seqConst: JobM<int> = JobM.result "x" >>-. 1
    let seqRaise: JobM<int> = JobM.result () >>-! Exception()
    let seqBindMemo: Promise<int> = JobM.result 1 >>=* fun x -> JobM.result (x + 1)
    let seqMapMemo: Promise<int> = JobM.result 1 >>-* ((+) 1)
    let seqThenMemo: Promise<int> = JobM.result () >>=*. JobM.result 2
    let seqConstMemo: Promise<int> = JobM.result "x" >>-*. 1
    let seqRaiseMemo: Promise<int> = JobM.result () >>-*! Exception()

    let kleisli: JobM<int> = (JobM.result >=> fun x -> JobM.result (x + 1)) 1
    let kleisliMap: JobM<int> = (JobM.result >-> ((+) 1)) 1
    let kleisliThen: JobM<int> = (JobM.result >=>. JobM.result 2) ()
    let kleisliConst: JobM<int> = (JobM.result >->. 2) ()
    let kleisliRaise: JobM<int> = (JobM.result >->! Exception()) ()
    let kleisliMemo: Promise<int> = (JobM.result >=>* fun x -> JobM.result (x + 1)) 1
    let kleisliMapMemo: Promise<int> = (JobM.result >->* ((+) 1)) 1
    let kleisliThenMemo: Promise<int> = (JobM.result >=>*. JobM.result 2) ()
    let kleisliConstMemo: Promise<int> = (JobM.result >->*. 2) ()
    let kleisliRaiseMemo: Promise<int> = (JobM.result >->*! Exception()) ()

    let pairSeq: JobM<int * int> = JobM.result 1 <&> JobM.result 2
    let pairPar: JobM<int * int> = JobM.result 1 <*> JobM.result 2
    let pairAlt: AltM<int * int> = AltM.always 1 <+> AltM.always 2

    let altAfterJob: AltM<int> = AltM.always 1 ^=> fun x -> JobM.result (x + 1)
    let altAfterFun: AltM<int> = AltM.always 1 ^-> ((+) 1)
    let altAfterThen: AltM<int> = AltM.always () ^=>. JobM.result 2
    let altAfterConst: AltM<int> = AltM.always () ^->. 2
    let altAfterRaise: AltM<int> = AltM.always () ^->! Exception()
    let altChoice: AltM<int> = AltM.always 1 <|> AltM.never ()
    let altChoiceMemo: Promise<int> = AltM.always 1 <|>* AltM.never ()
    let altRandom: AltM<int> = AltM.always 1 <~> AltM.never ()
    let altRandomMemo: Promise<int> = AltM.always 1 <~>* AltM.never ()

    let chGive: AltM<unit> = Ch() *<- 1
    let chSend: JobM<unit> = Ch() *<+ 1
    let ivarFill: JobM<unit> = IVar() *<= 1
    let ivarFail: JobM<unit> = IVar() *<=! Exception()
    let mvarFill: JobM<unit> = MVar() *<<= 1
    let mbSend: JobM<unit> = Mailbox() *<<+ 1

    let queryNackJob: AltM<int> =
        Ch() *<+->= fun (rCh: Ch<int>) _nack -> JobM.result rCh

    let queryNackFun: AltM<int> =
        Ch() *<+->- fun (rCh: Ch<int>) _nack -> rCh

    let queryGiveJob: AltM<int> =
        Ch() *<-=>= fun (rI: IVar<int>) -> JobM.result rI

    let queryGiveFun: AltM<int> =
        Ch() *<-=>- fun (rI: IVar<int>) -> rI

    let querySendJob: AltM<int> =
        Ch() *<+=>= fun (rI: IVar<int>) -> JobM.result rI

    let querySendFun: AltM<int> =
        Ch() *<+=>- fun (rI: IVar<int>) -> rI
