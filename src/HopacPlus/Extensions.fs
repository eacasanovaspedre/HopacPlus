namespace HopacPlus.Extensions

#nowarn "44"

open System
open System.Threading
open HopacPlus

module HopacSeq = Hopac.Extensions.Seq
module HopacArray = Hopac.Extensions.Array
module HopacAsync = Hopac.Extensions.Async

/// <summary>
/// Operations for processing sequences with jobs.
/// </summary>
module Seq =
    /// <summary>
    /// Sequentially maps the given job constructor to the elements of the
    /// sequence and returns a list of the results.
    /// </summary>
    let inline mapJob ([<InlineIfLambda>] x2yJ) xs = HopacSeq.mapJob (Job.toHopacF x2yJ) xs |> Job

    /// <summary>
    /// Sequentially iterates the given job constructor over the given sequence.
    /// </summary>
    let inline iterJob ([<InlineIfLambda>] x2uJ) xs = HopacSeq.iterJob (Job.toHopacF x2uJ) xs |> Job

    /// <summary>
    /// <c>Seq.iterJobIgnore x2yJ xs</c> is equivalent to <c>Seq.iterJob (x2yJ &gt;&gt;
    /// Job.Ignore) xs</c>.
    /// </summary>
    let inline iterJobIgnore ([<InlineIfLambda>] x2yJ) xs = HopacSeq.iterJobIgnore (Job.toHopacF x2yJ) xs |> Job

    /// <summary>
    /// Sequentially folds the job constructor over the given sequence and
    /// returns the result of the fold.
    /// </summary>
    let inline foldJob ([<InlineIfLambda>] x2y2xJ) x ys =
        HopacSeq.foldJob (fun x y -> x2y2xJ x y |> Job.toHopac) x ys |> Job

    /// <summary>
    /// <c>foldFromJob x x2y2xJ ys</c> is equivalent to <c>foldJob x2y2xJ x ys</c>.
    /// </summary>
    let inline foldFromJob x ([<InlineIfLambda>] x2y2xJ) ys =
        HopacSeq.foldFromJob x (fun x y -> x2y2xJ x y |> Job.toHopac) ys |> Job

    /// <summary>
    /// Operations for processing sequences using concurrent jobs.
    /// </summary>
    module Con =
        /// <summary>
        /// Iterates the given job constructor over the given sequence, runs the
        /// constructed jobs as separate concurrent jobs and waits until all of
        /// the jobs have finished collecting the results into a list.
        /// </summary>
        let inline mapJob ([<InlineIfLambda>] x2yJ) xs = HopacSeq.Con.mapJob (Job.toHopacF x2yJ) xs |> Job

        /// <summary>
        /// Iterates the given job constructor over the given sequence, runs the
        /// constructed jobs as separate concurrent jobs and waits until all of
        /// the jobs have finished.
        /// </summary>
        let inline iterJob ([<InlineIfLambda>] x2uJ) xs = HopacSeq.Con.iterJob (Job.toHopacF x2uJ) xs |> Job

        /// <summary>
        /// <c>Con.iterJobIgnore x2yJ xs</c> is equivalent to <c>Con.iterJob (x2yJ &gt;&gt;
        /// Job.Ignore) xs</c>.
        /// </summary>
        let inline iterJobIgnore ([<InlineIfLambda>] x2yJ) xs = HopacSeq.Con.iterJobIgnore (Job.toHopacF x2yJ) xs |> Job

/// <summary>Operations for processing arrays with jobs.</summary>
module Array =
    /// <summary>
    /// Sequentially maps the given job constructor to the elements of the array
    /// and returns an array of the results.
    /// </summary>
    let inline mapJob ([<InlineIfLambda>] x2yJ) xs = HopacArray.mapJob (Job.toHopacF x2yJ) xs |> Job

    /// <summary>
    /// Sequentially iterates the given job constructor over the given array.
    /// </summary>
    let inline iterJob ([<InlineIfLambda>] x2uJ) xs = HopacArray.iterJob (Job.toHopacF x2uJ) xs |> Job

    /// <summary>
    /// <c>Array.iterJobIgnore x2yJ xs</c> is equivalent to <c>Array.iterJob (x2yJ &gt;&gt;
    /// Job.Ignore) xs</c>.
    /// </summary>
    let inline iterJobIgnore ([<InlineIfLambda>] x2yJ) xs = HopacArray.iterJobIgnore (Job.toHopacF x2yJ) xs |> Job

/// <summary>Operations for interfacing F# async operations with jobs.</summary>
module Async =
    /// <summary>
    /// Creates a job that starts the given async operation and then waits until
    /// the operation finishes.
    /// </summary>
    let inline toJob xA = HopacAsync.toJob xA |> Job

    /// <summary>
    /// Creates a job that posts the given async operation to the specified
    /// synchronization context for execution and then waits until the operation
    /// finishes.
    /// </summary>
    let inline toJobOn ctx xA = HopacAsync.toJobOn ctx xA |> Job

    /// <summary>
    /// Creates an alternative that, when instantiated, starts the given async
    /// operation and then becomes enabled once the operation finishes.
    /// </summary>
    let inline toAlt xA = HopacAsync.toAlt xA |> Alt

    /// <summary>
    /// Creates an alternative that, when instantiated, posts the given async
    /// operation to the specified synchronization context for execution and
    /// then becomes enabled once the operation finishes.
    /// </summary>
    let inline toAltOn ctx xA = HopacAsync.toAltOn ctx xA |> Alt

    /// <summary>
    /// Creates an async operation that starts the given job on the specified
    /// scheduler and then waits until the started job finishes.
    /// </summary>
    let inline ofJobOn sr xJ = HopacAsync.ofJobOn sr (Job.toHopac xJ)

    /// <summary>Gets the main synchronization context.</summary>
    let inline getMain () = HopacAsync.getMain ()

    /// <summary>Sets the main synchronization context.</summary>
    let inline setMain ctx = HopacAsync.setMain ctx

    /// <summary>Operations on the global scheduler.</summary>
    module Global =
        /// <summary>
        /// Creates an async operation that starts the given job on the global
        /// scheduler and then waits until the started job finishes.
        /// </summary>
        let inline ofJob xJ = HopacAsync.Global.ofJob (Job.toHopac xJ)

        /// <summary>
        /// Creates a builder for running an async workflow on the main
        /// synchronization context and interoperating with the Hopac global
        /// scheduler.
        /// </summary>
        let inline onMain () = HopacAsync.Global.onMain ()

    /// <summary>
    /// Builder for an async operation started on the given synchronization
    /// context with jobs on the specified scheduler wrapped as a job.
    /// </summary>
    let inline asyncOn ctx sr = Hopac.Extensions.asyncOn ctx sr

/// <summary>
/// Creates a job that starts the given job as a separate concurrent job,
/// whose result can be obtained from the returned task.
/// </summary>
module Task =
    open Hopac.Extensions

    /// <summary>
    /// Creates a job that starts the given job as a separate concurrent job,
    /// whose result can be obtained from the returned task.
    /// </summary>
    let inline startJob xJ = System.Threading.Tasks.Task.startJob (Job.toHopac xJ) |> Job

/// <summary>
/// Raised by onceAltOn when the associated observable signals the OnCompleted
/// event.
/// </summary>
type OnCompleted = Hopac.Extensions.OnCompleted

/// <summary>Extensions for IObservable.</summary>
module Observable =
    open Hopac.Extensions

    /// <summary>
    /// <c>xO.onceAlt</c> is equivalent to <c>xO.onceAltOn null</c>.
    /// </summary>
    let inline onceAlt (xO: IObservable<'x>) = xO.onceAlt |> Alt

    /// <summary>
    /// Creates an alternative that, when instantiated, subscribes to the
    /// observable on the specified synchronization context for at most one
    /// event.
    /// </summary>
    let inline onceAltOn (ctx: SynchronizationContext) (xO: IObservable<'x>) = xO.onceAltOn ctx |> Alt

    /// <summary>
    /// This is equivalent to calling onceAltOn with the main synchronization
    /// context.
    /// </summary>
    let inline onceAltOnMain (xO: IObservable<'x>) = xO.onceAltOnMain |> Alt
