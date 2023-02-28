#lang scribble/doc
@(require scribble/manual
          scribble/example
          scriblib/footnote
          (for-label racket/base esterel/full))
@(define esterel-eval (make-base-eval '(require esterel/full)))
@(define-syntax-rule (ex expr ...) (examples #:eval esterel-eval #:label #f #:no-prompt expr ...))

@title{Esterel in Racket, for Racket Programmers}

Esterel is a synchronous reactive programming language that
dates to 1983. It has imperative features combined with
parallelism and the ability to abort already-running
computation, but Esterel is deterministic, with every
program guaranteed to produce its particular result no
matter how the various threads run or which threads are
aborted.

One way to think of imperative computation is to imagine
that all of the current values of all of the variables in
the program, collected together, as a single state and steps
in the computation changing this state as the program
executes. In this sense, Esterel is an imperative
programming language but, unlike more conventional
imperative programming languages, Esterel's state changes
are more restricted, giving the programmer more guarantees
about the behavior of the program.

Computation in Esterel is broken up in a series of
@deftech{instants} (also called @deftech{reactions}). Within
each instant, the entire program always sees a coherent,
single state. That is, within an instant there are no state
changes; instead each instant computes a new state and the
entire Esterel program sees that only that new state.
Furthermore, each instant is explicitly represented
in the program via the primitive operation @racket[pause].
In particular, each thread must, after some finite amount of
time, either invoke @racket[pause] (or terminate or be
aborted) and the time between these pauses always a single
consistent state, visible to the entire computation.

Of course, Esterel enforcement of determinacy during each
@tech{instant} applies only to Esterel programs and Esterel
threads; Racket's threads are not required to explicitly
pause and remain as complex and difficult to reason about as
ever. As such, the Esterel portion of the computation is
dynamically segregated from the ordinary Racket portion,
using the keyword @racket[reaction]. Also, in Esterel, the
program variables that make up the changing, mutable state
of the program are called signals to avoid possible
confusion.@note{Esterel computations do not check or
 guarantee that Racket code run during an instant is pure,
 and surprising behavior may result if Racket code uses
 mutation while it is running in an Esterel context. We
 return to this point later; for now, simply imagine only
 pure Racket code running alongside Esterel.}

@section{A Traffic Light}

To make this abstract notation of state change and pauses
and instants more concrete, let's look at some code that
controls a traffic signal with three lights: red, orange,
and green. Red means that cars should stop; orange means
that red is coming soon so either slow in preparation to
stop or finish transiting the intersection, and green means
that it is safe to transit the intersection.

To control the lights we will use three signals. A
signal in Esterel has two states: either it is present or it
is absent. Signals can also carry values when they are
present, but to control our traffic light, we'll just use
presence to indicate that the light should be on and absence
to indicate that it should be off. Here's how we declare the
signals:

@ex[
 (eval:no-prompt
  (define-signal red orange green))]

This introduces three new Racket identifiers, @racket[red],
@racket[orange], and @racket[green], bound to signals.

To run an Esterel program, we need to wrap the code in
@racket[reaction]. Any code or helper functions can be
invoked from within a reaction; the restriction that Esterel
code must be inside a reaction is a dynamic restriction. For
starters, let's just make a traffic signal that is
permanently green. Here's the code to do that:

@ex[
 (eval:no-prompt
  (define forever-red
    (reaction
     (let loop ()
       (emit red)
       (pause)
       (loop)))))
 ]

Looking at the code line by line, we see @racket[reaction],
which wraps our Esterel program, then a recursive loop
definition, followed by @racket[(emit green)] which causes
the signal @racket[green] to be emitted and thus present.
Then @racket[(pause)], indicating that the instant is
over. Once the next instant starts, it will pick up right at
the @racket[pause], and go back around the loop, emitting
the green signal again and pausing.

This code does not actually run yet, however. To run the
code, we need to use @racket[react!]. Each time we invoke
@racket[react!], a single @tech{instant} runs and
@racket[react!] returns the signals that were emitted,
always @racket[green].

@ex[
 (react! forever-red)
 (react! forever-red)
 (react! forever-red)
 ]

In Esterel, repeatedly emitting a specific signal and
pausing is a common pattern and is captured via the function
@racket[sustain]. Accordingly, this is the same program, but
written more compactly:

@ex[
 (eval:no-prompt
  (define forever-red
    (reaction
     (sustain red))))
 ]

Of course, we would like our traffic signal to change color
so cars can pass through the intersection. Let's say that
our green should last two minutes, and then orange for four
seconds, and then we'll change to red. To set this up, we'll
introduce a signal that is emitted every second:

@ex[
 (eval:no-prompt
  (define-signal second))
 ]

And now we turn to the loop that emits
@racket[forever-read]. A Rackety approach to this problem
might be to change the loop so that it has a parameter and
it counts the number of instants that passed where
@racket[second] is emitted, and based on that, emit the
correct signal.

Instead, and because aborting is completely safe and
reliable in Esterel, we can separate the code that is
emitting the signal from the code that decides how long to
wait, letting those the two tasks of emitting the red light
and deciding when it is done be handled in parallel, like
this:

@racketblock[
 (par (abort (sustain red)
             #:when (present? next-stage))
      (begin (await (present? second) #:n (* 4 60))
             (emit next-stage)))
   ]

The Esterel form @racket[par] runs each of its
subexpressions (in this case, the @racket[abort] and the
@racket[begin]) in parallel to each other; it finishes when
both of them finish. The @racket[abort] runs its first
argument (the call to @racket[sustain]) until the code
following the @racket[#:when] returns @racket[#t], at which
point it aborts it, which terminates the first branch of the
@racket[par]. Meanwhile, the second branch of the
@racket[par] runs the @racket[await], which waits until
there have been @racket[(* 4 60)] instants when
@racket[second] is present. After that, the @racket[await]
call returns, in that same instant, the
@racket[(emit next-stage)] happens, ending the entire
expression.

Let's wrap all this up into a helper function:
@ex[
 (eval:no-prompt
  (define (traffic-light-stage color seconds)
    (let-signal (next-stage)
      (par (abort (sustain color)
                  #:when (present? next-stage))
           (begin (await (present? second) #:n seconds)
                  (emit next-stage))))))
 ]

and use @racket[let-signal] to make a local signal,
@racket[next-stage], which is available only in the body.

Now, let's run a reaction that repeatedly runs the traffic light
@ex[
 (eval:no-prompt
  (define three-stages
    (reaction
     (traffic-light-stage green (* 4 60))
     (traffic-light-stage orange 4)
     (traffic-light-stage red 2))))
 (react! three-stages #:emit (list second))
 (react! three-stages #:emit (list second))
 (for ([i (in-range (* 4 60))])
   (react! three-stages #:emit (list second)))
 (react! three-stages #:emit (list second))
 (react! three-stages #:emit (list second))
 (react! three-stages #:emit (list second))
 ]

This code runs the @racket[sustain] call until the signal
@racket[next-stage] is present, at which point it aborts the
loop inside @racket[sustain] and we continue on to whatever
code follows. Then, we can, in parallel to that loop make
write code that will emit @racket[next-stage] when enough
time has passed, using @racket[await].

@section{Causality}

Inside a single instant, instead of a totally ordered linear
notion of time we have, instead, a weaker notion of time,
called causality, that determines the flow of the
computation.

In order to see how these aspects of Esterel computation
play out, let's start with a simple program. In this
program, because @racket[S] is not set, the program takes
the else branch of the conditional and emits @racket[O2].

@examples[#:label #f #:eval esterel-eval #:no-prompt
          (define-signal S O1 O2)
          (react!
           (reaction
            (if (present? S)
                (emit O1)
                (emit O2))))]

We can see the effect of @racket[S] not being emitted and
@racket[O2] being emitted in the result of @racket[react!],
as the hash maps @racket[S] to @racket[#f] and @racket[O2]
to @racket[#t].

If we add a call to @racket[emit] in sequence (i.e., before) the
conditional, as in this program, we causes the conditional
to take the other branch:
@examples[#:label #f #:eval esterel-eval #:no-prompt
          (react!
           (reaction
            (emit S)
            (if (present? S)
                (emit O1)
                (emit O2))))]

But Esterel does not require the emission to be sequentially ordered
with the @racket[present?] test. Since Esterel's notion of
parallelism is deterministic, this program is guaranteed to
behave as the previous one, always emitting @racket[O1] and
never emitting @racket[O2], even though the test for
@racket[S] is done in parallel to the emission of
@racket[S].

@examples[#:label #f #:eval esterel-eval #:no-prompt
          (react! (reaction
                   (par (emit S)
                        (if (present? S)
                            (emit O1)
                            (emit O2)))))]

To understand this, we must return to the weaker notion of
time, namely causality. That is, instead of a conventional
understanding of the two parallel branches of this
@racket[par] as running at the same time as each other,
another way to think of them is that there is no causality
relationship between the two evaluations and thus Esterel is
free to run them however it wants, subject to other
causality dependencies being obeyed. In other words, to
Esterel, the difference between combining two expression in
parallel or sequentially is that combining them sequentially
forces a causality ordering on them, but combining them in
parallel does not.

Even though @racket[par] did not introduce a causal
dependency in this program, there still is a causality
dependency introduced. Specifically, each use of
@racket[emit] introduces a causality dependency between it
and any uses of @racket[present?] (for the same signal). So,
in this program, @racket[(emit S)] @emph{causes}
the call @racket[(present? #t)] to return @racket[#true] and
therefore therefore we can take only the branch that emits
@racket[O1] and not the one that emits
@racket[O2].
