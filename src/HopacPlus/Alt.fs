namespace HopacPlus

open System.Threading
open System.Threading.Tasks
open Hopac

module HopacJob = Hopac.Job
module HopacAlt = Hopac.Alt
module HopacPromise = Hopac.Promise

/// <summary>An Alt wrapped so FSharpPlus can resolve map / bind / empty / (&lt;|&gt;).</summary>
[<Struct>]
type Alt<'T> =
    | Alt of HopacAlt<'T>

    static member ToHopac(Alt a) : HopacAlt<'T> = a

[<Struct>]
type Promise<'T> =
    | Promise of HopacPromise<'T>

    static member ToHopac(Promise p) : HopacPromise<'T> = p

/// <summary>
/// Operations on first-class synchronous operations or alternatives.
/// </summary>
module Alt =
    let inline toHopac (x: ^a) : #HopacAlt<'t> = (^a: (static member ToHopac: ^a -> #HopacAlt<'t>) x)

    let inline internal toHopacF ([<InlineIfLambda>] f) x = x |> f |> toHopac

    /// <summary>
    /// Creates an alternative that is always available and results in the given
    /// value.
    /// Note that when there are alternatives immediately available in a choice,
    /// the first such alternative will be committed to.
    /// </summary>
    let inline always x = HopacAlt.always x |> Alt

    /// <summary>
    /// Returns an alternative that is always available and results in the unit
    /// value.  <c>unit ()</c> is an optimized version of <c>always ()</c>.
    /// </summary>
    let inline unit () = HopacAlt.unit () |> Alt

    /// <summary>
    /// Returns an alternative that can be committed to once and that produces the
    /// given value.
    /// </summary>
    let inline once x = HopacAlt.once x |> Alt

    /// <summary>
    /// Creates an alternative that is never available.
    /// Note that synchronizing on <c>never ()</c>, without other alternatives is
    /// equivalent to performing <c>abort ()</c>.
    /// </summary>
    let inline never () = HopacAlt.never () |> Alt

    /// <summary>
    /// Returns an alternative that is never available.  <c>zero ()</c> is an optimized
    /// version of <c>never ()</c>.
    /// </summary>
    let inline zero () = HopacAlt.zero () |> Alt

    /// <summary>
    /// Creates an alternative that has the effect of raising the specified
    /// exception.  <c>raises e</c> is equivalent to <c>prepareFun &lt;| fun () -&gt; raise e</c>.
    /// </summary>
    let inline raises e = HopacAlt.raises e |> Alt

    /// <summary>
    /// <c>Ignore xA</c> is equivalent to <c>xA ^-&gt; always ()</c>.
    /// </summary>
    let inline Ignore (x: '``Alt<'x>``) = x |> toHopac |> HopacAlt.Ignore |> Alt

    /// <summary>
    /// Creates an alternative which is committed to when the given alternative
    /// is committed to. Once committed, the given alternative's result is mapped
    /// using the given function, providing the final result.
    /// <c>xA |&gt; afterFun x2y</c> is equivalent to <c>xA |&gt; afterJob (x2y &gt;&gt; result)</c>.
    /// This is the same as <c>^-&gt;</c> with the arguments flipped.
    /// </summary>
    let inline afterFun ([<InlineIfLambda>] x2y: 'x -> 'y) (x: '``Alt<'x>``) =
        x |> toHopac |> HopacAlt.afterFun x2y |> Alt

    /// <summary>
    /// Creates an alternative whose result is passed to the given job constructor
    /// and processed with the resulting job after the given alternative has been
    /// committed to.  This is the same as <c>^=&gt;</c> with the arguments flipped.
    /// </summary>
    let inline afterJob ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``) (x: '``Alt<'x>``) : Alt<'y> =
        HopacAlt.afterJob (Job.toHopacF x2yJ) (toHopac x) |> Alt

    /// <summary>
    /// Creates an alternative that is available when any one of the given
    /// alternatives is.  See also: choosy, <c>&lt;|&gt;</c>.
    /// Note that <c>choose []</c> is equivalent to <c>never ()</c>.
    /// </summary>
    let inline choose (xAs: '``Alt<'x>`` seq) : Alt<'x> = xAs |> Seq.map toHopac |> HopacAlt.choose |> Alt

    /// <summary>
    /// <c>choosy xAs</c> (read: choose array) is an optimized version of <c>choose xAs</c>
    /// when <c>xAs</c> is an array.  Do not write <c>choosy (Seq.toArray xAs)</c> instead
    /// of <c>choose xAs</c> unless the resulting alternative is reused many times.
    /// </summary>
    let inline choosy (xAs: '``Alt<'x>`` array) : Alt<'x> =
        Array.init xAs.Length (fun i -> xAs[i] |> toHopac) |> HopacAlt.choosy |> Alt

    /// <summary>
    /// <c>chooser xAs</c> is like <c>choose xAs</c> except that the order in which the
    /// alternatives from the sequence are instantiated will be determined at
    /// random each time the alternative is used.  See also: <c>&lt;~&gt;</c>.
    /// </summary>
    let inline chooser (xAs: '``Alt<'x>`` seq) : Alt<'x> = xAs |> Seq.map toHopac |> HopacAlt.chooser |> Alt

    /// <summary>
    /// Creates an alternative computed at instantiation time with
    /// the given function, which will be called with a pseudo random 64-bit
    /// unsigned integer.  See also: Random.bind.
    /// </summary>
    let inline random ([<InlineIfLambda>] r2xA: uint64 -> '``Alt<'x>``) : Alt<'x> =
        r2xA |> toHopacF |> HopacAlt.random |> Alt

    /// <summary>
    /// Creates an alternative computed at instantiation time with the
    /// given thunk.  See also: <c>*&lt;-=&gt;-</c>, prepareJob.
    /// </summary>
    let inline prepareFun ([<InlineIfLambda>] u2xA: unit -> '``Alt<'x>``) : Alt<'x> =
        u2xA |> toHopacF |> HopacAlt.prepareFun |> Alt

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given job.  See also: <c>*&lt;-=&gt;-</c>, prepareFun, withNackJob.
    /// </summary>
    let inline prepareJob ([<InlineIfLambda>] u2xAJ: unit -> '``Job<Alt<'x>>``) : Alt<'x> =
        fun () -> () |> u2xAJ |> Job.toHopac |> HopacJob.map toHopac
        |> HopacAlt.prepareJob
        |> Alt

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given job.  <c>prepare xAJ</c> is equivalent to <c>prepareJob &lt;| fun () -&gt; xAJ</c>.
    /// </summary>
    let inline prepare (xAJ: '``Job<Alt<'x>>``) : Alt<'x> =
        xAJ |> Job.toHopac |> HopacJob.map toHopac |> HopacAlt.prepare |> Alt

    /// <summary>
    /// Creates an alternative that is computed at instantiation time with the
    /// given job constructed with a negative acknowledgment alternative.  See
    /// also: <c>*&lt;+-&gt;-</c>, withNackFun, prepareJob.
    /// </summary>
    let inline withNackJob ([<InlineIfLambda>] n2xAJ: Promise<unit> -> '``Job<Alt<'x>>``) : Alt<'x> =
        fun nack -> nack |> Promise |> n2xAJ |> Job.toHopac |> HopacJob.map toHopac
        |> HopacAlt.withNackJob
        |> Alt

    /// <summary>
    /// <c>withNackFun n2xA</c> is equivalent to <c>withNackJob (Job.lift n2xA)</c>.
    /// </summary>
    let inline withNackFun ([<InlineIfLambda>] n2xA: Promise<unit> -> '``Alt<'x>``) : Alt<'x> =
        (fun nack -> nack |> Promise |> n2xA |> toHopac) |> HopacAlt.withNackFun |> Alt

    /// <summary>
    /// Returns a new alternative that makes it so that the given job will be
    /// started as a separate concurrent job if the given alternative isn't the
    /// one being committed to.  See also: wrapAbortFun, withNackJob.
    /// </summary>
    let inline wrapAbortJob (uJ: '``Job<unit>``) (x: '``Alt<'x>``) : Alt<'x> =
        HopacAlt.wrapAbortJob (Job.toHopac uJ) (toHopac x) |> Alt

    /// <summary>
    /// <c>wrapAbortFun u2u xA</c> is equivalent to <c>wrapAbortJob (Job.thunk u2u) xA</c>.
    /// </summary>
    let inline wrapAbortFun ([<InlineIfLambda>] u2u) (x: '``Alt<'x>``) : Alt<'x> =
        HopacAlt.wrapAbortFun u2u (toHopac x) |> Alt

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
    let inline tryIn
        (x: '``Alt<'x>``)
        ([<InlineIfLambda>] x2yJ: 'x -> '``Job<'y>``)
        ([<InlineIfLambda>] e2yJ: exn -> '``Job<'y>``)
        : Alt<'x> =
        HopacAlt.tryIn (toHopac x) (Job.toHopacF x2yJ) (Job.toHopacF e2yJ) |> Alt

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for alternatives.  The given action, specified as a function, is executed
    /// after the alternative has been committed to, whether the alternative fails
    /// or completes successfully.  Note that the action is not executed in case
    /// the alternative is not committed to.  Use withNackJob to attach the
    /// action to the non-committed case.
    /// </summary>
    let inline tryFinallyFun (x: '``Alt<'x>``) ([<InlineIfLambda>] u2u) : Alt<'x> =
        HopacAlt.tryFinallyFun (toHopac x) u2u |> Alt

    /// <summary>
    /// Implements a variation of the <c>try-finally</c> exception handling construct
    /// for alternatives.  The given action, specified as a job, is executed after
    /// the alternative has been committed to, whether the alternative fails or
    /// completes successfully.  Note that the action is not executed in case the
    /// alternative is not committed to.  Use withNackJob to attach the action
    /// to the non-committed case.
    /// </summary>
    let inline tryFinallyJob (x: '``Alt<'x>``) (uJ: '``Job<unit>``) : Alt<'x> =
        HopacAlt.tryFinallyJob (toHopac x) (Job.toHopac uJ) |> Alt

    /// <summary>
    /// Creates an alternative that, when instantiated, starts the cancellable
    /// asynchronous operation defined by the given <c>doBegin</c>, <c>doEnd</c> and
    /// <c>doCancel</c> operations and waits for it to complete, after which the
    /// alternative becomes available.  If some other alternative is committed to
    /// a in a choice before the operation completes, then the operation is
    /// cancelled. See also: Job.fromBeginEnd.
    /// </summary>
    let inline fromBeginEnd doBegin doEnd doCancel = HopacAlt.fromBeginEnd doBegin doEnd doCancel |> Alt

    /// <summary>
    /// Creates an alternative that, when instantiated, starts the given
    /// cancellable async operation and waits for it to complete, after which the
    /// alternative becomes available.  If some other alternative is committed to
    /// in a choice before the operation completes, then the operation is
    /// cancelled.  See also: Job.fromAsync.
    /// </summary>
    let inline fromAsync xA = HopacAlt.fromAsync xA |> Alt

    /// <summary>
    /// Creates an async operation that starts the given alternative and waits for
    /// it to be committed to.  If the async operation is cancelled before the
    /// alternative is committed to, an attempt is made to also cancel the
    /// alternative by making a cancellation alternative available.  Note that
    /// cancellation is not transactional and <c>Alt.toAsync &gt;&gt; Alt.fromAsync</c> is
    /// not the identity function.  See also: Job.toAsync.
    /// </summary>
    let inline toAsync (x: '``Alt<'x>``) : Async<'x> = HopacAlt.toAsync (toHopac x)

    /// <summary>
    /// Creates an alternative that, when instantiated, calls the given function
    /// with a cancellation token to start a cancellable task and waits for it to
    /// complete, after which the alternative becomes available.  If some other
    /// alternative is committed to in a choice before the task completes, then
    /// the token will be cancelled.  See also: Job.fromTask.
    /// </summary>
    let inline fromTask ([<InlineIfLambda>] u2xT: CancellationToken -> Task<_>) = HopacAlt.fromTask u2xT |> Alt

    /// <summary>
    /// Creates an alternative that, when instantiated, calls the given function
    /// with a cancellation token to start a cancellable task and waits for it to
    /// complete, after which the alternative becomes available.  If some other
    /// alternative is committed to in a choice before the task completes, then
    /// the token will be cancelled.  See also: Job.fromUnitTask.
    /// </summary>
    let inline fromUnitTask ([<InlineIfLambda>] u2uT: CancellationToken -> Task) = HopacAlt.fromUnitTask u2uT |> Alt

    /// <summary>
    /// Given an alternative, creates a new alternative that behaves exactly like
    /// the given alternative, except that the new alternative obviously cannot be
    /// directly downcast to the underlying type of the given alternative.  This
    /// operation is provided for debugging purposes.  You can always break
    /// abstractions using reflection.  See also: Job.paranoid.
    /// </summary>
    let inline paranoid (x: '``Alt<'x>``) : Alt<'x> = HopacAlt.paranoid (toHopac x) |> Alt

    /// <summary>
    /// Creates an alternative that yields the thread of execution to any ready
    /// work and then becomes available.
    /// </summary>
    let idle = Alt Hopac.idle

type Alt<'T> with
    static member inline Return x = Alt.always x
    static member inline Map(x, [<InlineIfLambda>] f) = Alt.afterFun f x

    static member inline (>>=)(x, [<InlineIfLambda>] f) = Alt.afterJob f x

    static member inline Delay([<InlineIfLambda>] f) = Alt.prepareFun f

    static member inline Empty() : Alt<'T> = Alt.never ()
    static member inline get_Empty() : Alt<'T> = Alt.never ()
    static member inline (<|>)(Alt x, Alt y) = Infixes.(<|>) x y
