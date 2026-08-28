namespace HopacPlus

open HopacPlus

module HopacStream = Hopac.Stream

/// <summary>A point in a non-deterministic stream of values.</summary>
[<Struct>]
type Cons<'T> =
    | Cons of Hopac.Stream.Cons<'T>

    static member inline ToHopac(Cons c) : Hopac.Stream.Cons<'T> = c

/// <summary>A non-deterministic stream of values called a choice stream.</summary>
type Stream<'T> = Promise<Hopac.Stream.Cons<'T>>

/// <summary>A stream source.</summary>
[<Struct>]
type Src<'T> =
    | Src of Hopac.Stream.Src<'T>

    static member inline ToHopac(Src s) : Hopac.Stream.Src<'T> = s

/// <summary>A stream variable.</summary>
[<Struct>]
type Var<'T> =
    | Var of Hopac.Stream.Var<'T>

    static member inline ToHopac(Var v) : Hopac.Stream.Var<'T> = v

/// <summary>A stream serialized variable.</summary>
[<Struct>]
type StreamMVar<'T> =
    | StreamMVar of Hopac.Stream.MVar<'T>

    static member inline ToHopac(StreamMVar m) : Hopac.Stream.MVar<'T> = m

/// <summary>Operations on choice streams.</summary>
module Stream =
    let inline private ofHopac s : Stream<'x> = Promise s
    let inline private toHopac (s: Stream<'x>) = Promise.toHopac s

    /// <summary>Communicates the end of the stream.</summary>
    let inline nil<'x> : Stream<'x> = HopacStream.nil |> ofHopac

    /// <summary>A stream that never produces a value.</summary>
    let inline never<'x> : Stream<'x> = HopacStream.never |> ofHopac

    /// <summary>A stream of a single value.</summary>
    let inline one x = HopacStream.one x |> ofHopac

    /// <summary>Repeats the given value indefinitely.</summary>
    let inline repeat x = HopacStream.repeat x |> ofHopac

    /// <summary>A stream that fails with the given exception.</summary>
    let inline error e = HopacStream.error e |> ofHopac

    /// <summary>Creates a stream from the given sequence.</summary>
    let inline ofSeq xs = HopacStream.ofSeq xs |> ofHopac

    /// <summary>Prepends a value to the given stream.</summary>
    let inline cons x xs = HopacStream.cons x (toHopac xs) |> ofHopac

    /// <summary>Delays construction of a stream.</summary>
    let inline delay ([<InlineIfLambda>] u2xs) = HopacStream.delay (fun () -> u2xs () |> Job.toHopac) |> ofHopac

    /// <summary>A stream that produces a single value from the given job.</summary>
    let inline once (xJ: '``Job<'x>``) = HopacStream.once (Job.toHopac xJ) |> ofHopac

    /// <summary>Repeatedly runs the given job to produce stream elements.</summary>
    let inline indefinitely (xJ: '``Job<'x>``) = HopacStream.indefinitely (Job.toHopac xJ) |> ofHopac

    let inline unfoldFun ([<InlineIfLambda>] s2xso) s = HopacStream.unfoldFun s2xso s |> ofHopac

    let inline unfoldJob ([<InlineIfLambda>] s2xsoJ) s = HopacStream.unfoldJob (Job.toHopacF s2xsoJ) s |> ofHopac

    let inline iterateFun ([<InlineIfLambda>] x2x) x = HopacStream.iterateFun x2x x |> ofHopac

    let inline iterateJob ([<InlineIfLambda>] x2xJ) x = HopacStream.iterateJob (Job.toHopacF x2xJ) x |> ofHopac

    let inline generateFun s ([<InlineIfLambda>] s2b) ([<InlineIfLambda>] s2s) ([<InlineIfLambda>] s2x) =
        HopacStream.generateFun s s2b s2s s2x |> ofHopac

    let inline generateFuns s funs = HopacStream.generateFuns s funs |> ofHopac

    let inline afterDateTimeOffset dto = HopacStream.afterDateTimeOffset dto |> ofHopac

    let inline afterDateTimeOffsets xs = HopacStream.afterDateTimeOffsets (toHopac xs) |> ofHopac

    let inline afterTimeSpan ts = HopacStream.afterTimeSpan ts |> ofHopac

    let inline mapFun ([<InlineIfLambda>] x2y) xs = HopacStream.mapFun x2y (toHopac xs) |> ofHopac

    let inline mapJob ([<InlineIfLambda>] x2yJ) xs = HopacStream.mapJob (Job.toHopacF x2yJ) (toHopac xs) |> ofHopac

    let inline mapConst y xs = HopacStream.mapConst y (toHopac xs) |> ofHopac

    let inline mapIgnore xs = HopacStream.mapIgnore (toHopac xs) |> ofHopac

    let inline mapPipelinedFun n ([<InlineIfLambda>] x2y) xs = HopacStream.mapPipelinedFun n x2y (toHopac xs) |> ofHopac

    let inline mapPipelinedJob n ([<InlineIfLambda>] x2yJ) xs =
        HopacStream.mapPipelinedJob n (Job.toHopacF x2yJ) (toHopac xs) |> ofHopac

    let inline filterFun ([<InlineIfLambda>] x2b) xs = HopacStream.filterFun x2b (toHopac xs) |> ofHopac

    let inline filterJob ([<InlineIfLambda>] x2bJ) xs =
        HopacStream.filterJob (Job.toHopacF x2bJ) (toHopac xs) |> ofHopac

    let inline choose xs = HopacStream.choose (toHopac xs) |> ofHopac

    let inline chooseFun ([<InlineIfLambda>] x2yo) xs = HopacStream.chooseFun x2yo (toHopac xs) |> ofHopac

    let inline chooseJob ([<InlineIfLambda>] x2yoJ) xs =
        HopacStream.chooseJob (Job.toHopacF x2yoJ) (toHopac xs) |> ofHopac

    let inline take n xs = HopacStream.take n (toHopac xs) |> ofHopac

    let inline skip n xs = HopacStream.skip n (toHopac xs) |> ofHopac

    let inline takeWhileFun ([<InlineIfLambda>] x2b) xs = HopacStream.takeWhileFun x2b (toHopac xs) |> ofHopac

    let inline takeWhileJob ([<InlineIfLambda>] x2bJ) xs =
        HopacStream.takeWhileJob (Job.toHopacF x2bJ) (toHopac xs) |> ofHopac

    let inline skipWhileFun ([<InlineIfLambda>] x2b) xs = HopacStream.skipWhileFun x2b (toHopac xs) |> ofHopac

    let inline skipWhileJob ([<InlineIfLambda>] x2bJ) xs =
        HopacStream.skipWhileJob (Job.toHopacF x2bJ) (toHopac xs) |> ofHopac

    let inline append xs ys = HopacStream.append (toHopac xs) (toHopac ys) |> ofHopac

    let inline appendMap ([<InlineIfLambda>] x2ys) xs = HopacStream.appendMap (x2ys >> toHopac) (toHopac xs) |> ofHopac

    let inline appendAll xss = HopacStream.appendMap toHopac (toHopac xss) |> ofHopac

    let inline merge xs ys = HopacStream.merge (toHopac xs) (toHopac ys) |> ofHopac

    let inline mergeMap ([<InlineIfLambda>] x2ys) xs = HopacStream.mergeMap (x2ys >> toHopac) (toHopac xs) |> ofHopac

    let inline mergeAll xss = HopacStream.mergeMap toHopac (toHopac xss) |> ofHopac

    let inline amb xs ys = HopacStream.amb (toHopac xs) (toHopac ys) |> ofHopac

    let inline ambMap ([<InlineIfLambda>] x2ys) xs = HopacStream.ambMap (x2ys >> toHopac) (toHopac xs) |> ofHopac

    let inline ambAll xss = HopacStream.ambMap toHopac (toHopac xss) |> ofHopac

    let inline switch xs ys = HopacStream.switch (toHopac xs) (toHopac ys) |> ofHopac

    let inline switchTo ys xs = HopacStream.switchTo (toHopac ys) (toHopac xs) |> ofHopac

    let inline switchMap ([<InlineIfLambda>] x2ys) xs = HopacStream.switchMap (x2ys >> toHopac) (toHopac xs) |> ofHopac

    let inline switchAll xss = HopacStream.switchMap toHopac (toHopac xss) |> ofHopac

    let inline zip xs ys = HopacStream.zip (toHopac xs) (toHopac ys) |> ofHopac

    let inline zipWithFun ([<InlineIfLambda>] x2y2z) xs ys =
        HopacStream.zipWithFun x2y2z (toHopac xs) (toHopac ys) |> ofHopac

    let inline combineLatest xs ys = HopacStream.combineLatest (toHopac xs) (toHopac ys) |> ofHopac

    let inline scanFun ([<InlineIfLambda>] s2x2s) s xs = HopacStream.scanFun s2x2s s (toHopac xs) |> ofHopac

    let inline scanJob ([<InlineIfLambda>] s2x2sJ) s xs =
        HopacStream.scanJob (fun s x -> s2x2sJ s x |> Job.toHopac) s (toHopac xs)
        |> ofHopac

    let inline scanFromFun s ([<InlineIfLambda>] s2x2s) xs = HopacStream.scanFromFun s s2x2s (toHopac xs) |> ofHopac

    let inline scanFromJob s ([<InlineIfLambda>] s2x2sJ) xs =
        HopacStream.scanFromJob s (fun s x -> s2x2sJ s x |> Job.toHopac) (toHopac xs)
        |> ofHopac

    let inline buffer n xs = HopacStream.buffer n (toHopac xs) |> ofHopac

    let inline distinctByFun ([<InlineIfLambda>] x2k) xs = HopacStream.distinctByFun x2k (toHopac xs) |> ofHopac

    let inline distinctByJob ([<InlineIfLambda>] x2kJ) xs =
        HopacStream.distinctByJob (Job.toHopacF x2kJ) (toHopac xs) |> ofHopac

    let inline distinctUntilChanged xs = HopacStream.distinctUntilChanged (toHopac xs) |> ofHopac

    let inline distinctUntilChangedByFun ([<InlineIfLambda>] x2k) xs =
        HopacStream.distinctUntilChangedByFun x2k (toHopac xs) |> ofHopac

    let inline distinctUntilChangedByJob ([<InlineIfLambda>] x2kJ) xs =
        HopacStream.distinctUntilChangedByJob (Job.toHopacF x2kJ) (toHopac xs)
        |> ofHopac

    let inline distinctUntilChangedWithFun ([<InlineIfLambda>] x2x2b) xs =
        HopacStream.distinctUntilChangedWithFun x2x2b (toHopac xs) |> ofHopac

    let inline distinctUntilChangedWithJob ([<InlineIfLambda>] x2x2bJ) xs =
        HopacStream.distinctUntilChangedWithJob (fun x y -> x2x2bJ x y |> Job.toHopac) (toHopac xs)
        |> ofHopac

    let inline groupByFun ([<InlineIfLambda>] k2uJ2xs2y) ([<InlineIfLambda>] x2k) xs =
        HopacStream.groupByFun (fun k uJ ys -> k2uJ2xs2y k (Job uJ) (ofHopac ys)) x2k (toHopac xs)
        |> ofHopac

    let inline groupByJob ([<InlineIfLambda>] k2uJ2xs2yJ) ([<InlineIfLambda>] x2kJ) xs =
        HopacStream.groupByJob
            (fun k uJ ys -> k2uJ2xs2yJ k (Job uJ) (ofHopac ys) |> Job.toHopac)
            (Job.toHopacF x2kJ)
            (toHopac xs)
        |> ofHopac

    let inline foldJob ([<InlineIfLambda>] s2x2sJ) s xs =
        Job (HopacStream.foldJob (fun s x -> s2x2sJ s x |> Job.toHopac) s (toHopac xs))

    let inline foldFun ([<InlineIfLambda>] s2x2s) s xs = Job (HopacStream.foldFun s2x2s s (toHopac xs))

    let inline foldFromJob s ([<InlineIfLambda>] s2x2sJ) xs =
        Job (HopacStream.foldFromJob s (fun s x -> s2x2sJ s x |> Job.toHopac) (toHopac xs))

    let inline foldFromFun s ([<InlineIfLambda>] s2x2s) xs = Job (HopacStream.foldFromFun s s2x2s (toHopac xs))

    let inline foldBack ([<InlineIfLambda>] x2sJ2s) xs s =
        HopacStream.foldBack (fun x sJ -> x2sJ2s x (Promise sJ) |> Job.toHopac) (toHopac xs) (Job.toHopac s)
        |> Promise

    let inline foldFromBack s ([<InlineIfLambda>] xsJ2x2s) xs =
        HopacStream.foldFromBack (Job.toHopac s) (fun xsJ x -> xsJ2x2s (Promise xsJ) x |> Job.toHopac) (toHopac xs)
        |> Promise

    let inline count xs = Job (HopacStream.count (toHopac xs))

    let inline iter xs = Job (HopacStream.iter (toHopac xs))

    let inline iterFun ([<InlineIfLambda>] x2u) xs = Job (HopacStream.iterFun x2u (toHopac xs))

    let inline iterJob ([<InlineIfLambda>] x2uJ) xs = Job (HopacStream.iterJob (Job.toHopacF x2uJ) (toHopac xs))

    let inline consume xs = HopacStream.consume (toHopac xs)

    let inline consumeFun ([<InlineIfLambda>] x2u) xs = HopacStream.consumeFun x2u (toHopac xs)

    let inline consumeJob ([<InlineIfLambda>] x2uJ) xs = HopacStream.consumeJob (Job.toHopacF x2uJ) (toHopac xs)

    let inline tryPickFun ([<InlineIfLambda>] x2yo) xs = Job (HopacStream.tryPickFun x2yo (toHopac xs))

    let inline tryPickJob ([<InlineIfLambda>] x2yoJ) xs = Job (HopacStream.tryPickJob (Job.toHopacF x2yoJ) (toHopac xs))

    let inline head xs = HopacStream.head (toHopac xs) |> ofHopac

    let inline tail xs = HopacStream.tail (toHopac xs) |> ofHopac

    let inline last xs = HopacStream.last (toHopac xs) |> ofHopac

    let inline init xs = HopacStream.init (toHopac xs) |> ofHopac

    let inline inits xs = HopacStream.inits (toHopac xs) |> ofHopac

    let inline tails xs = HopacStream.tails (toHopac xs) |> ofHopac

    let inline initsMapFun ([<InlineIfLambda>] xs2y) xs =
        HopacStream.initsMapFun (fun s -> xs2y (ofHopac s)) (toHopac xs) |> ofHopac

    let inline tailsMapFun ([<InlineIfLambda>] xs2y) xs =
        HopacStream.tailsMapFun (fun s -> xs2y (ofHopac s)) (toHopac xs) |> ofHopac

    let inline toSeq xs = Job (HopacStream.toSeq (toHopac xs))

    let inline values xs = HopacStream.values (toHopac xs) |> Alt

    let inline cycle xs = HopacStream.cycle (toHopac xs) |> ofHopac

    let inline catch ([<InlineIfLambda>] e2xs) xs = HopacStream.catch (e2xs >> toHopac) (toHopac xs) |> ofHopac

    let inline onCloseFun ([<InlineIfLambda>] u2u) xs = HopacStream.onCloseFun u2u (toHopac xs) |> ofHopac

    let inline onCloseJob (uJ: '``Job<unit>``) xs = HopacStream.onCloseJob (Job.toHopac uJ) (toHopac xs) |> ofHopac

    let inline doFinalizeFun ([<InlineIfLambda>] u2u) xs = HopacStream.doFinalizeFun u2u (toHopac xs) |> ofHopac

    let inline doFinalizeJob (uJ: '``Job<unit>``) xs =
        HopacStream.doFinalizeJob (Job.toHopac uJ) (toHopac xs) |> ofHopac

    let inline ofObservable xO = HopacStream.ofObservable xO |> ofHopac

    let inline ofObservableOn ctx xO = HopacStream.ofObservableOn ctx xO |> ofHopac

    let inline ofObservableOnMain xO = HopacStream.ofObservableOnMain xO |> ofHopac

    let inline toObservable xs = HopacStream.toObservable (toHopac xs)

    let inline shift (xJ: '``Job<_>``) xs = HopacStream.shift (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline debounce (xA: '``Alt<_>``) xs = HopacStream.debounce (Alt.toHopac xA) (toHopac xs) |> ofHopac

    let inline ignoreUntil (xJ: '``Job<_>``) xs = HopacStream.ignoreUntil (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline ignoreWhile (xJ: '``Job<_>``) xs = HopacStream.ignoreWhile (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline samplesBefore xs ys = HopacStream.samplesBefore (toHopac xs) (toHopac ys) |> ofHopac

    let inline samplesAfter xs ys = HopacStream.samplesAfter (toHopac xs) (toHopac ys) |> ofHopac

    let inline skipUntil (xA: '``Alt<_>``) xs = HopacStream.skipUntil (Alt.toHopac xA) (toHopac xs) |> ofHopac

    let inline takeUntil (xA: '``Alt<_>``) xs = HopacStream.takeUntil (Alt.toHopac xA) (toHopac xs) |> ofHopac

    let inline takeAndSkipUntil (xA: '``Alt<_>``) xs =
        let a, b = HopacStream.takeAndSkipUntil (Alt.toHopac xA) (toHopac xs)
        ofHopac a, ofHopac b

    let inline keepPreceding n xs = HopacStream.keepPreceding n (toHopac xs) |> ofHopac

    let inline keepPreceding1 xs = HopacStream.keepPreceding1 (toHopac xs) |> ofHopac

    let inline keepFollowing1 xs = HopacStream.keepFollowing1 (toHopac xs) |> ofHopac

    let inline keepPrecedingFuns funs xs = HopacStream.keepPrecedingFuns funs (toHopac xs) |> ofHopac

    let inline afterEach (xJ: '``Job<_>``) xs = HopacStream.afterEach (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline beforeEach (xJ: '``Job<_>``) xs = HopacStream.beforeEach (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline delayEach (xJ: '``Job<_>``) xs = HopacStream.delayEach (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline duringEach (xJ: '``Job<_>``) xs = HopacStream.duringEach (Job.toHopac xJ) (toHopac xs) |> ofHopac

    let inline pullOn xs ys = HopacStream.pullOn (toHopac xs) (toHopac ys) |> ofHopac

    let inline append' xs ys = HopacStream.append' (Alt.toHopac xs) (Alt.toHopac ys) |> Alt

    let inline merge' xs ys = HopacStream.merge' (Alt.toHopac xs) (Alt.toHopac ys) |> Alt

    let inline amb' xs ys = HopacStream.amb' (Alt.toHopac xs) (Alt.toHopac ys) |> Alt

    let inline switch' xs ys = HopacStream.switch' (Alt.toHopac xs) (Alt.toHopac ys) |> Alt

    /// <summary>
    /// Joins all the streams in the given stream of streams together with the
    /// given binary join combinator primitive.
    /// </summary>
    let inline joinWith ([<InlineIfLambda>] y2xs2zJ) ys =
        HopacStream.joinWith (fun y xsA -> y2xs2zJ y (Alt xsA) |> Job.toHopac) (toHopac ys)
        |> ofHopac

    /// <summary>
    /// <c>mapJoin j f xs</c> is equivalent to <c>joinWith j &lt;| mapFun f xs</c>.
    /// </summary>
    let inline mapJoin ([<InlineIfLambda>] y2xs2zJ) ([<InlineIfLambda>] x2y) xs =
        HopacStream.mapJoin (fun y xsA -> y2xs2zJ y (Alt xsA) |> Job.toHopac) x2y (toHopac xs)
        |> ofHopac

    /// <summary>Generator functions for generateFuns.</summary>
    type GenerateFuns<'s, 'x> = Hopac.Stream.GenerateFuns<'s, 'x>

    /// <summary>Functions for collecting elements from a live stream to be lazified.</summary>
    type KeepPrecedingFuns<'x, 'y> = Hopac.Stream.KeepPrecedingFuns<'x, 'y>

    /// <summary>
    /// A mutable property, much like a stream variable, that generates a stream
    /// of values and property change notifications as a side-effect.
    /// </summary>
    type Property<'x> = Hopac.Stream.Property<'x>

    /// <summary>Operations on stream properties.</summary>
    module Property =
        /// <summary>Creates a new property with the specified initial value.</summary>
        let inline create x = Hopac.Stream.Property x

        /// <summary>Gets the value of the property.</summary>
        let inline get (p: Property<'x>) = p.Value

        /// <summary>Sets the value of the property.</summary>
        let inline set (p: Property<'x>) x = p.Value <- x

        /// <summary>
        /// Returns the generated stream, including the current value of the
        /// property, from the point in time when tap is called.
        /// </summary>
        let inline tap (p: Property<'x>) = p.Tap () |> ofHopac

    /// <summary>Operations on stream sources.</summary>
    module Src =
        let inline create () = HopacStream.Src.create () |> Src

        let inline value (s: Src<'x>) x =
            let (Src inner) = s
            Job (HopacStream.Src.value inner x)

        let inline error (s: Src<'x>) e =
            let (Src inner) = s
            Job (HopacStream.Src.error inner e)

        let inline close (s: Src<'x>) =
            let (Src inner) = s
            Job (HopacStream.Src.close inner)

        let inline tap (s: Src<'x>) =
            let (Src inner) = s
            HopacStream.Src.tap inner |> ofHopac

    /// <summary>Operations on stream variables.</summary>
    module Var =
        let inline create x = HopacStream.Var.create x |> Var

        let inline get (v: Var<'x>) =
            let (Var inner) = v
            HopacStream.Var.get inner

        let inline set (v: Var<'x>) x =
            let (Var inner) = v
            Job (HopacStream.Var.set inner x)

        let inline tap (v: Var<'x>) =
            let (Var inner) = v
            HopacStream.Var.tap inner |> ofHopac

    /// <summary>Operations on stream serialized variables.</summary>
    module MVar =
        let inline create x = HopacStream.MVar.create x |> StreamMVar

        let inline get (m: StreamMVar<'x>) =
            let (StreamMVar inner) = m
            Job (HopacStream.MVar.get inner)

        let inline set (m: StreamMVar<'x>) x =
            let (StreamMVar inner) = m
            Job (HopacStream.MVar.set inner x)

        let inline updateFun (m: StreamMVar<'x>) ([<InlineIfLambda>] x2x) =
            let (StreamMVar inner) = m
            Job (HopacStream.MVar.updateFun inner x2x)

        let inline updateJob (m: StreamMVar<'x>) ([<InlineIfLambda>] x2xJ) =
            let (StreamMVar inner) = m
            Job (HopacStream.MVar.updateJob inner (Job.toHopacF x2xJ))

        let inline tap (m: StreamMVar<'x>) =
            let (StreamMVar inner) = m
            HopacStream.MVar.tap inner |> ofHopac

    /// <summary>
    /// A generic builder for streams. Delegates Combine and Zero to the given
    /// Hopac stream builder so Bind, For and While keep consistent join
    /// semantics.
    /// </summary>
    [<Struct>]
    type Builder =
        | Builder of inner: HopacStream.Builder

        member inline this.Zero() : Stream<'x> = let (Builder inner) = this in inner.Zero () |> ofHopac

        member inline this.Yield(x) : Stream<'x> = let (Builder inner) = this in inner.Yield x |> ofHopac

        member inline this.YieldFrom(xs: Stream<'x>) = xs

        member inline this.Delay([<InlineIfLambda>] u2xs: unit -> Stream<'x>) = delay u2xs

        member inline this.Combine(xs: Stream<'x>, ys: Stream<'x>) : Stream<'x> =
            let (Builder inner) = this in inner.Combine (toHopac xs, toHopac ys) |> ofHopac

        member inline this.Combine'(xs: Alt<Hopac.Stream.Cons<'x>>, ys: Alt<Hopac.Stream.Cons<'x>>) =
            let (Builder inner) = this in inner.Combine' (Alt.toHopac xs, Alt.toHopac ys) |> Alt

        member inline this.Bind(xs: Stream<'x>, [<InlineIfLambda>] x2ys: 'x -> Stream<'y>) : Stream<'y> =
            let (Builder inner) = this in inner.Bind (toHopac xs, x2ys >> toHopac) |> ofHopac

        member inline this.For(xs: seq<'x>, [<InlineIfLambda>] x2ys: 'x -> Stream<'y>) : Stream<'y> =
            let (Builder inner) = this in inner.For (xs, x2ys >> toHopac) |> ofHopac

        member inline this.While(u2b, xs: Stream<'x>) : Stream<'x> = let (Builder inner) = this in inner.While (u2b, toHopac xs) |> ofHopac

        member inline this.TryWith(xs: Stream<'x>, [<InlineIfLambda>] e2xs) : Stream<'x> =
            let (Builder inner) = this in inner.TryWith (toHopac xs, e2xs >> toHopac) |> ofHopac

        member inline _.ReturnFrom(xs: Stream<'x>) = xs

    /// <summary>
    /// This builder joins substreams with amb' to produce a stream with the
    /// first results.
    /// </summary>
    let ambed = Builder HopacStream.ambed

    /// <summary>
    /// This builder joins substreams with append' to produce a stream with all
    /// results in sequential order.
    /// </summary>
    let appended = Builder HopacStream.appended

    /// <summary>
    /// This builder joins substreams with merge' to produce a stream with all
    /// results in completion order.
    /// </summary>
    let merged = Builder HopacStream.merged

    /// <summary>
    /// This builder joins substreams with switch' to produce a stream with the
    /// latest results.
    /// </summary>
    let switched = Builder HopacStream.switched

/// <summary>Expression builder type for streams.</summary>
type StreamBuilder = Stream.Builder

[<AutoOpen>]
module StreamTopLevel =
    /// <summary>
    /// Default stream builder. Joins substreams sequentially with append.
    /// </summary>
    let stream = Stream.appended
