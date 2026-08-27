module HopacPlus.Infixes

open Hopac
open HopacPlus

module HopacInfixes = Infixes

// <summary>
/// Creates an alternative that, using the given job constructor, constructs a
/// query with a reply channel and a nack, sends it to the query channel and
/// commits on taking the reply from the reply channel.  See also: <c>*&lt;+-&gt;-</c>.
/// </summary>
let inline ( *<+->= ) qCh ([<InlineIfLambda>] rCh2n2qJ) =
    Alt(
        HopacInfixes.( *<+->=)
            (Ch.toHopac qCh)
            (fun rCh nack -> Job.toHopac (rCh2n2qJ (Ch rCh) (Promise nack)))
    )

/// <summary>
/// Creates an alternative that, using the given function, constructs a query
/// with a reply channel and a nack, sends it to the query channel and commits
/// on taking the reply from the reply channel.  <c>*&lt;+-&gt;-</c> captures the most
/// common use case of <c>Alt.withNackJob</c> and is a slightly less expressive
/// form of <c>*&lt;+-&gt;=</c>.  See also: <c>*&lt;-=&gt;-</c>.
/// </summary>
let inline ( *<+->- ) qCh ([<InlineIfLambda>] rCh2n2q) =
    Alt(HopacInfixes.( *<+->-) (Ch.toHopac qCh) (fun rCh nack -> rCh2n2q (Ch rCh) (Promise nack)))

/// <summary>
/// Creates an alternative that, using the given job constructor, constructs a
/// query with a reply variable, commits on giving the query and reads the
/// reply variable.  See also: <c>*&lt;-=&gt;-</c>.
/// </summary>
let inline ( *<-=>= ) qCh ([<InlineIfLambda>] rI2qJ) =
    Alt(HopacInfixes.( *<-=>=) (Ch.toHopac qCh) (fun rI -> Job.toHopac (rI2qJ (IVar rI))))

/// <summary>
/// Creates an alternative that, using the given function, constructs a query
/// with a reply variable, commits on giving the query and reads the reply
/// variable.  <c>*&lt;-=&gt;-</c> captures the most common use case of
/// <c>Alt.prepareFun</c> and is a slightly less expressive form of <c>*&lt;-=&gt;=</c>.
/// See also: <c>*&lt;+-&gt;-</c>.
/// </summary>
let inline ( *<-=>- ) qCh ([<InlineIfLambda>] rI2q) =
    Alt(HopacInfixes.( *<-=>-) (Ch.toHopac qCh) (fun rI -> rI2q (IVar rI)))

/// <summary>
/// Creates an alternative that, using the given job constructor, constructs a
/// query with a reply variable, sends the query and reads the reply.  In
/// order for the alternative to make sense, the operation must not require
/// exclusive choice.  If this is not the case, then the resulting value
/// should only be used as a job.
/// </summary>
let inline ( *<+=>= ) qCh ([<InlineIfLambda>] rI2qJ) =
    Alt(HopacInfixes.( *<+=>=) (Ch.toHopac qCh) (fun rI -> Job.toHopac (rI2qJ (IVar rI))))

/// <summary>
/// Creates an alternative that, using the given function, constructs a query
/// with a reply variable, sends the query and reads the reply.  In order for
/// the alternative to make sense, the operation must not require exclusive
/// choice.  If this is not the case, then the resulting value should only be
/// used as a job.
/// </summary>
let inline ( *<+=>- ) qCh ([<InlineIfLambda>] rI2q) =
    Alt(HopacInfixes.( *<+=>-) (Ch.toHopac qCh) (fun rI -> rI2q (IVar rI)))

// Message passing

/// <summary>
/// Creates an alternative that, at instantiation time, offers to give the
/// given value on the given channel, and becomes available when another job
/// offers to take the value.  <c>xCh *&lt;- x</c> is equivalent to <c>Ch.give xCh x</c>.
/// </summary>
let inline ( *<- ) xCh x = Alt(HopacInfixes.( *<-) (Ch.toHopac xCh) x)

/// <summary>
/// Creates a job that sends a value to another job on the given channel.  A
/// send operation is asynchronous.  In other words, a send operation does not
/// wait for another job to give the value to.  <c>xCh *&lt;+ x</c> is equivalent to
/// <c>Ch.send xCh x</c>.
/// </summary>
let inline ( *<+ ) xCh x = Job(HopacInfixes.( *<+) (Ch.toHopac xCh) x)

/// <summary>
/// Creates a job that writes to the given write once variable.  It is an
/// error to write to a single <c>IVar</c> more than once.  <c>xI *&lt;= x</c> is
/// equivalent to <c>IVar.fill xI x</c>.
/// </summary>
let inline ( *<= ) xI x = Job(HopacInfixes.( *<=) (IVar.toHopac xI) x)

/// <summary>
/// Creates a job that writes the given exception to the given write once
/// variable.  It is an error to write to a single <c>IVar</c> more than once.
/// <c>xI *&lt;=! e</c> is equivalent to <c>IVar.fillFailure xI e</c>.
/// </summary>
let inline ( *<=! ) xI e = Job(HopacInfixes.( *<=!) (IVar.toHopac xI) e)

/// <summary>
/// Creates a job that writes the given value to the serialized variable.  It
/// is an error to write to a <c>MVar</c> that is full.  <c>xM *&lt;&lt;= x</c> is
/// equivalent to <c>MVar.fill xM x</c>.
/// </summary>
let inline ( *<<= ) xM x = Job(HopacInfixes.( *<<=) (MVar.toHopac xM) x)

/// <summary>
/// Creates a job that sends the given value to the specified mailbox.  This
/// operation never blocks.  <c>xMb *&lt;&lt;+ x</c> is equivalent to
/// <c>Mailbox.send xMb x</c>.
/// </summary>
let inline ( *<<+ ) xMb x = Job(HopacInfixes.( *<<+) (Mailbox.toHopac xMb) x)

// After actions

/// <summary>
/// Creates an alternative whose result is passed to the given job constructor
/// and processed with the resulting job after the given alternative has been
/// committed to.  This is the same as <c>afterJob</c> with the arguments flipped.
/// </summary>
let inline (^=>) xA ([<InlineIfLambda>] x2yJ) =
    Alt(HopacInfixes.(^=>) (Alt.toHopac xA) (Job.toHopacF x2yJ))

/// <summary>
/// Creates an alternative which is committed to when the given alternative
/// is committed to. Once committed, the given alternative's result is mapped
/// using the given function, providing the final result.
/// <c>xA ^-&gt; x2y</c> is equivalent to <c>xA ^=&gt; (x2y &gt;&gt; result)</c>.  This is the same
/// as <c>afterFun</c> with the arguments flipped.
/// </summary>
let inline (^->) xA ([<InlineIfLambda>] x2y) = Alt(HopacInfixes.(^->) (Alt.toHopac xA) x2y)

/// <summary>
/// Creates an alternative which is committed to when the given alternative
/// is committed to. Once committed, the job argument is executed and
/// generates the result.
/// <c>xA ^=&gt;. yJ</c> is equivalent to <c>xA ^=&gt; always yJ</c>.
/// </summary>
let inline (^=>.) xA yJ =
    Alt(HopacInfixes.(^=>.) (Alt.toHopac xA) (Job.toHopac yJ))

/// <summary>
/// Creates an alternative which is committed to when the given alternative
/// is committed to. Once committed, the given value is used as the result.
/// <c>xA ^-&gt;. y</c> is equivalent to <c>xA ^-&gt; always y</c>.
/// </summary>
let inline (^->.) xA y = Alt(HopacInfixes.(^->.) (Alt.toHopac xA) y)

/// <summary>
/// Creates an alternative which is committed to when the alternative
/// argument is committed to. Once committed, the given exception is raised.
/// <c>xA ^-&gt;! e</c> is equivalent to <c>xA ^-&gt; fun _ -&gt; raise e</c>.
/// </summary>
let inline (^->!) xA e = Alt(HopacInfixes.(^->!) (Alt.toHopac xA) e)

// Choices

/// <summary>
/// Creates an alternative that is available when either of the given
/// alternatives is available.  <c>xA1 &lt;|&gt; xA2</c> is an optimized version of
/// <c>choose [xA1; xA2]</c>.  See also: choosy.
/// </summary>
let inline (<|>) xA1 xA2 =
    Alt(HopacInfixes.(<|>) (Alt.toHopac xA1) (Alt.toHopac xA2))

/// <summary>A memoizing version of <c>&lt;|&gt;</c>.</summary>
let inline (<|>*) xA1 xA2 =
    HopacInfixes.(<|>*) (Alt.toHopac xA1) (Alt.toHopac xA2)

/// <summary>
/// <c>xA1 &lt;~&gt; xA2</c> is like <c>xA1 &lt;|&gt; xA2</c> except that the order in which
/// <c>xA1</c> and <c>xA2</c> are instantiated is determined at random every time the
/// alternative is used.  See also: chooser.
/// </summary>
let inline (<~>) xA1 xA2 =
    Alt(HopacInfixes.(<~>) (Alt.toHopac xA1) (Alt.toHopac xA2))

/// <summary>A memoizing version of <c>&lt;~&gt;</c>.</summary>
let inline (<~>*) xA1 xA2 =
    HopacInfixes.(<~>*) (Alt.toHopac xA1) (Alt.toHopac xA2)

// Sequencing

/// <summary>
/// Creates a job that first runs the given job and then passes the result of
/// that job to the given function to build another job which will then be
/// run.  This is the same as bind with the arguments flipped.
/// </summary>
let inline (>>=) (Job xJ) ([<InlineIfLambda>] x2yJ) =
    Job(HopacInfixes.(>>=) xJ (fun a -> let (Job y) = x2yJ a in y))

let inline megaBind xJ ([<InlineIfLambda>] x2yJ) =
    Job(HopacInfixes.(>>=) (Job.toHopac xJ) (fun a -> let (Job y) = x2yJ a in y))

/// <summary>A memoizing version of <c>&gt;&gt;=</c>.</summary>
let inline (>>=*) xJ ([<InlineIfLambda>] x2yJ) =
    HopacInfixes.(>>=*) (Job.toHopac xJ) (Job.toHopacF x2yJ)

/// <summary>
/// Creates a job that runs the given job and maps the result of the job with
/// the given function.  <c>xJ &gt;&gt;- x2y</c> is an optimized version of
/// <c>xJ &gt;&gt;= (x2y &gt;&gt; result)</c>.  This is the same as map with the arguments
/// flipped.
/// </summary>
let inline (>>-) (Job xJ) ([<InlineIfLambda>] x2y) = Job(HopacInfixes.(>>-) xJ x2y)

/// <summary>A memoizing version of <c>&gt;&gt;-</c>.</summary>
let inline (>>-*) xJ ([<InlineIfLambda>] x2y) = HopacInfixes.(>>-*) (Job.toHopac xJ) x2y

/// <summary>
/// Creates a job that runs the given two jobs and returns the result of the
/// second job.  <c>xJ &gt;&gt;=. yJ</c> is equivalent to <c>xJ &gt;&gt;= always yJ</c>.
/// </summary>
let inline (>>=.) (Job xJ) (Job yJ) = Job(HopacInfixes.(>>=.) xJ yJ)

/// <summary>A memoizing version of <c>&gt;&gt;=.</c>.</summary>
let inline (>>=*.) xJ yJ =
    HopacInfixes.(>>=*.) (Job.toHopac xJ) (Job.toHopac yJ)

/// <summary>
/// Creates a job that runs the given job and then returns the given value.
/// <c>xJ &gt;&gt;-. y</c> is an optimized version of <c>xJ &gt;&gt;= always (result y)</c>.
/// </summary>
let inline (>>-.) (Job xJ) y = Job(HopacInfixes.(>>-.) xJ y)

/// <summary>A memoizing version of <c>&gt;&gt;-.</c>.</summary>
let inline (>>-*.) xJ y = HopacInfixes.(>>-*.) (Job.toHopac xJ) y

/// <summary>
/// Creates a job that runs the given job and then raises the given exception.
/// <c>xJ &gt;&gt;-! e</c> is equivalent to <c>xJ &gt;&gt;= fun _ -&gt; raise e</c>.
/// </summary>
let inline (>>-!) (Job xJ) e = Job(HopacInfixes.(>>-!) xJ e)

/// <summary>A memoizing version of <c>&gt;&gt;-!</c>.</summary>
let inline (>>-*!) xJ e = HopacInfixes.(>>-*!) (Job.toHopac xJ) e

// Composition

/// <summary>
/// Creates a job that is the composition of the given two job constructors.
/// <c>(x2yJ &gt;=&gt; y2zJ) x</c> is equivalent to <c>x2yJ x &gt;&gt;= y2zJ</c> and is much like
/// the <c>&gt;&gt;</c> operator on ordinary functions.
/// </summary>
let inline (>=>) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2zJ) x =
    Job(
        HopacInfixes.(>=>)
            (fun a -> let (Job y) = x2yJ a in y)
            (fun b -> let (Job z) = y2zJ b in z)
            x
    )

/// <summary>A memoizing version of <c>&gt;=&gt;</c>.</summary>
let inline (>=>*) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2zJ) x =
    HopacInfixes.(>=>*) (Job.toHopacF x2yJ) (Job.toHopacF y2zJ) x

/// <summary>
/// Creates a job that is the composition of the given job constructor and
/// function.  <c>(x2yJ &gt;-&gt; y2z) x</c> is equivalent to <c>x2yJ x &gt;&gt;- y2z</c> and is
/// much like the <c>&gt;&gt;</c> operator on ordinary functions.
/// </summary>
let inline (>->) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2z) x =
    Job(HopacInfixes.(>->) (fun a -> let (Job y) = x2yJ a in y) y2z x)

/// <summary>A memoizing version of <c>&gt;-&gt;</c>.</summary>
let inline (>->*) ([<InlineIfLambda>] x2yJ) ([<InlineIfLambda>] y2z) x =
    HopacInfixes.(>->*) (Job.toHopacF x2yJ) y2z x

/// <summary>
/// <c>(x2yJ &gt;=&gt;. zJ) x</c> is equivalent to <c>x2yJ x &gt;&gt;=. zJ</c>.
/// </summary>
let inline (>=>.) ([<InlineIfLambda>] x2yJ) zJ x =
    let (Job z) = zJ
    Job(HopacInfixes.(>=>.) (fun a -> let (Job y) = x2yJ a in y) z x)

/// <summary>A memoizing version of <c>&gt;=&gt;.</c>.</summary>
let inline (>=>*.) ([<InlineIfLambda>] x2yJ) zJ x =
    HopacInfixes.(>=>*.) (Job.toHopacF x2yJ) (Job.toHopac zJ) x

/// <summary>
/// <c>(x2yJ &gt;-&gt;. z) x</c> is equivalent to <c>x2yJ x &gt;&gt;-. z</c>.
/// </summary>
let inline (>->.) ([<InlineIfLambda>] x2yJ) z x =
    Job(HopacInfixes.(>->.) (fun a -> let (Job y) = x2yJ a in y) z x)

/// <summary>A memoizing version of <c>&gt;-&gt;.</c>.</summary>
let inline (>->*.) ([<InlineIfLambda>] x2yJ) z x = HopacInfixes.(>->*.) (Job.toHopacF x2yJ) z x

/// <summary>
/// <c>(x2yJ &gt;-&gt;! e) x</c> is equivalent to <c>x2yJ x &gt;&gt;-! e</c>.
/// </summary>
let inline (>->!) ([<InlineIfLambda>] x2yJ) e x =
    Job(HopacInfixes.(>->!) (fun a -> let (Job y) = x2yJ a in y) e x)

/// <summary>A memoizing version of <c>&gt;-&gt;!</c>.</summary>
let inline (>->*!) ([<InlineIfLambda>] x2yJ) e x = HopacInfixes.(>->*!) (Job.toHopacF x2yJ) e x

// Pairing

/// <summary>
/// Creates a job that runs the given two jobs and then returns a pair of
/// their results.  <c>xJ &lt;&amp;&gt; yJ</c> is equivalent to
/// <c>xJ &gt;&gt;= fun x -&gt; yJ &gt;&gt;= fun y -&gt; result (x, y)</c>.
/// </summary>
let inline (<&>) xJ yJ =
    let (Job x) = xJ
    let (Job y) = yJ
    Job(HopacInfixes.(<&>) x y)

/// <summary>
/// Creates a job that either runs the given jobs sequentially, like
/// <c>&lt;&amp;&gt;</c>, or as two separate parallel jobs and returns a pair of their
/// results.  This is Hopac's pairing operator, not FSharpPlus applicative
/// apply.
/// </summary>
let inline (<*>) xJ yJ =
    let (Job x) = xJ
    let (Job y) = yJ
    Job(HopacInfixes.(<*>) x y)

/// <summary>
/// An alternative that is equivalent to first committing to either one of the
/// given alternatives and then committing to the other alternative.  Note
/// that this is not the same as committing to both of the alternatives in a
/// single transaction.
/// </summary>
let inline (<+>) xA yA =
    Alt(HopacInfixes.(<+>) (Alt.toHopac xA) (Alt.toHopac yA))
