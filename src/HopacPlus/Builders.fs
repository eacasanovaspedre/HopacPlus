namespace HopacPlus

open System
open HopacPlus.Extensions

module HopacJob = Hopac.Job
module HopacAlt = Hopac.Alt
module HopacPromise = Hopac.Promise

/// <summary>Expression builder type for jobs.</summary>
type JobBuilder() =
    member inline _.Bind(x, [<InlineIfLambda>] f) = Job.bind f x

    member inline _.Bind(x, [<InlineIfLambda>] f) = Job.bind f (Job x)

    member inline _.Bind(x, [<InlineIfLambda>] f) = Job.bindAsync f x

    member inline _.Bind(x, [<InlineIfLambda>] f) = Job.bindTask f x

    member inline _.Bind(x, [<InlineIfLambda>] f) = Job.bindUnitTask f x

    member inline this.Bind(x: IObservable<'x>, [<InlineIfLambda>] f) = this.Bind (Observable.onceAlt x, f)

    member inline _.Combine(a: Job<unit>, [<InlineIfLambda>] b: unit -> Job<'x>) : Job<'x> = Job.bind (fun () -> b ()) a

    member inline _.Delay
        ([<InlineIfLambda>] u2xJ: unit -> '``Job<'x>`` when '``Job<'x>``: (static member ToHopac: ^a -> #HopacJob<'t>))
        =
        u2xJ

    member inline _.For(xs: seq<'x>, [<InlineIfLambda>] body: 'x -> Job<unit>) : Job<unit> = Seq.iterJob body xs

    member inline _.Return(x: 'x) : Job<'x> = Job.result x

    member inline _.ReturnFrom(x) = Job.toJob x

    member inline _.ReturnFrom(x) : Job<'x> = Job x

    member inline _.ReturnFrom(x) = Job.fromAsync x

    member inline _.ReturnFrom(x) = Job.awaitTask x

    member inline _.ReturnFrom(x) = Job.awaitUnitTask x

    member inline _.ReturnFrom(x: IObservable<'x>) = let (Alt a) = Observable.onceAlt x in Job a

    member inline _.Run([<InlineIfLambda>] u2xJ) = Job.delay u2xJ

    member inline _.TryFinally([<InlineIfLambda>] u2xJ, [<InlineIfLambda>] compensation) =
        Job.tryFinallyFunDelay u2xJ compensation

    member inline _.TryWith([<InlineIfLambda>] u2xJ, [<InlineIfLambda>] handler) = Job.tryWithDelay u2xJ handler

    member inline _.Using(resource: #IDisposable, [<InlineIfLambda>] body) = Job.using resource body

    member inline _.While([<InlineIfLambda>] guard, [<InlineIfLambda>] body) = Job.whileDoDelay guard body

    member inline _.Zero() = Job.unit ()
