namespace HopacPlus

open Hopac

module HopacScheduler = Hopac.Scheduler

/// <summary>Operations on schedulers.</summary>
module Scheduler =
    /// <summary>Creates a new local scheduler.</summary>
    let inline create opts = HopacScheduler.create opts

    /// <summary>
    /// Kills the worker threads of the scheduler one-by-one.  This should only be
    /// used with a local scheduler that is known to be idle.
    /// </summary>
    let inline kill sr = HopacScheduler.kill sr

    /// <summary>
    /// Waits until the scheduler becomes completely idle.
    /// </summary>
    let inline wait sr = HopacScheduler.wait sr

    /// <summary>
    /// Starts running the given job on the specified scheduler and then blocks
    /// the current thread waiting for the job to either return successfully or
    /// fail.
    /// </summary>
    let inline run sr x = HopacScheduler.run sr (Job.toHopac x)

    /// <summary>
    /// Starts running the given job, but does not wait for the job to finish.
    /// Upon the failure or success of the job, one of the given actions is called
    /// once.
    /// </summary>
    let inline startWithActions sr e2u x2u x = HopacScheduler.startWithActions sr e2u x2u (Job.toHopac x)

    /// <summary>
    /// <c>startIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; start</c>.
    /// </summary>
    let inline startIgnore sr xJ = HopacScheduler.startIgnore sr (Job.toHopac xJ)

    /// <summary>
    /// Starts running the given job, but does not wait for the job to finish.
    /// </summary>
    let inline start sr uJ = HopacScheduler.start sr (Job.toHopac uJ)

    /// <summary>
    /// Like start, but the given job is known never to return normally.
    /// </summary>
    let inline server sr xJ = HopacScheduler.server sr (Job.toHopac xJ)

    /// <summary>
    /// <c>queueIgnore xJ</c> is equivalent to <c>Job.Ignore xJ |&gt; queue</c>.
    /// </summary>
    let inline queueIgnore sr xJ = HopacScheduler.queueIgnore sr (Job.toHopac xJ)

    /// <summary>Queues the given job for execution on the scheduler.</summary>
    let inline queue sr uJ = HopacScheduler.queue sr (Job.toHopac uJ)

    /// <summary>Operations on the global scheduler.</summary>
    module Global =
        /// <summary>
        /// Sets options for creating the global scheduler.  This must be called
        /// before invoking any Hopac functionality that implicitly creates the
        /// global scheduler.
        /// </summary>
        let inline setCreate opts = HopacScheduler.Global.setCreate opts
