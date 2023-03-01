#lang scribble/doc
@(require scribble/manual
          scribble/example
          scriblib/footnote
          "bib.rkt"
          (for-label racket/base esterel/full))
@(define esterel-eval (make-base-eval '(require esterel/full)))
@(define-syntax-rule (ex expr ...)
   (examples #:eval esterel-eval #:label #f
             expr ...))

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
time, invoke @racket[pause] (or terminate or be
aborted) and the time between these pauses always has a single
consistent state, visible to the entire computation.

Of course, Esterel's enforcement of determinacy during each
@tech{instant} applies only to Esterel programs and Esterel
threads; Racket's threads are not required to explicitly
pause and remain as complex and difficult to reason about as
ever.

Separating Esterel computation from normal imperative Racket
computation, however, is simply a matter of wrapping
@racket[esterel] around some expressions. Any code that runs
in the dynamic extent of those expressions is treated as
Esterel code. Unfortunately, ordinary Racket mutable
variables do not automatically become Esterel-like; instead,
the Esterel code must use @tech{signal}s for any values that
change over time. Unfortunately, Esterel computations do not
check or guarantee that Racket code run during an instant is
otherwise pure, and surprising behavior may result if Racket
code uses mutation while it is running in an Esterel
context. We return to this point in @secref["sec:conts"];
for now, simply imagine only pure Racket code running
alongside Esterel.

@section{A Traffic Light}

To make this abstract notation of state change and pauses
and instants more concrete, let's look at some code that
controls a traffic signal with three lights: red, orange,
and green. Red means that cars should stop; orange means
that red is coming soon, so either slow in preparation to
stop, or finish transiting the intersection; green means
that it is safe to transit the intersection.

To control the lights we will use three signals. A
@deftech{signal} in Esterel has two states: either it is
present or it is absent. Signals can also carry values when
they are present, but to control our traffic light, we'll
just use presence to indicate that the light should be on
and absence to indicate that it should be off. Here's how we
declare the signals:

@ex[
 (eval:no-prompt
  (define-signal red orange green))]

This introduces three new Racket identifiers, @racket[red],
@racket[orange], and @racket[green], each bound to a signal.

To run an Esterel program, we need to wrap the code in
@racket[esterel]. Any code or helper functions can be
invoked from inside. That is, Esterel
code runs in the dynamic extent of @racket[esterel]. For
starters, let's just make a traffic signal that is
permanently red. Here's the code to do that:

@ex[
 (eval:no-prompt
  (define forever-red
    (esterel
     (let loop ()
       (emit red)
       (pause)
       (loop)))))
 ]

Looking at the code line by line, we see @racket[esterel],
which wraps our Esterel program, then a recursive loop
definition, followed by @racket[(emit red)] which causes
the signal @racket[red] to be emitted and thus present.
Then @racket[(pause)], indicating that the instant is
over. Once the next instant starts, it will pick up right at
the @racket[pause], and go back around the loop, emitting
the red signal again and pausing.

This code does not actually run yet, however. To run the
code, we need to use @racket[react!]. Each time we invoke
@racket[react!], a single @tech{instant} runs and
@racket[react!] returns the signals that were emitted,
always @racket[red].

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
    (esterel
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
wait, letting those the two tasks be handled in parallel,
like this:

@racketblock[
 (par (abort (sustain red)
             #:when (present? next-stage))
      (begin (await (present? second) #:n (* 4 60))
             (emit next-stage)))
   ]

The Esterel form @racket[par] runs each of its
subexpressions (in this case, the @racket[abort] and the
@racket[begin]) in parallel to each other; it finishes only
after both of them finish. The @racket[abort] runs its first
argument (the call to @racket[sustain]) until the code
following the @racket[#:when] returns @racket[#t], at which
point it aborts it. That abort is the only way to terminates
the first branch of the @racket[par]. Meanwhile, the second
branch of the @racket[par] runs the @racket[await], which
waits until there have been @racket[(* 4 60)] instants when
@racket[second] is present. After that, the @racket[await]
call returns and, in that same instant, emits
@racket[next-stage] happens, ending the entire @racket[par]
expression.

Let's wrap all this up into a helper function:
@ex[
 (eval:no-prompt
  (define (traffic-light-stage color seconds)
    (with-signal (next-stage)
      (par (abort (sustain color)
                  #:when (present? next-stage))
           (begin (await (present? second) #:n seconds)
                  (emit next-stage))))))
 ]

and use @racket[with-signal] to make a local signal,
@racket[next-stage], which is available only in the body of
the @racket[with-signal].

Now, we can write a reaction that repeatedly runs the traffic light:
@ex[
 (eval:no-prompt
  (define three-stages
    (esterel
     (traffic-light-stage green (* 4 60))
     (traffic-light-stage orange 4)
     (traffic-light-stage red 2))))
]

And if we use @racket[react!]'s @racket[#:emit] keyword argument
to supply the @racket[second] signal, we can see the light changing
@ex[
 (react! three-stages #:emit (list second))
 (react! three-stages #:emit (list second))
 (for ([i (in-range (* 4 60))])
   (react! three-stages #:emit (list second)))
 (react! three-stages #:emit (list second))
 (react! three-stages #:emit (list second))
 (react! three-stages #:emit (list second))
 ]

For a more developed version of this example with some GUI
code to see the traffic light change color, see
@tt{esterel/examples/traffic-light}.

@section{Causality}

There is a subtle point about the semantics of Esterel and
how it manages to give the each instant its own consistent
world view. One way to understand it is to think that each
instant, instead of a totally ordered linear notion of time
it has, instead, a weaker notion of time, called causality.
That is, we do not know (or need to care) exactly which
events came before or after which other ones; instead we
need to know only which events cause other events: that is
enough to determine the flow of the computation.

In order to see how these aspects of Esterel computation
play out, let's start with a simple program. In this
program, because @racket[S] is not set, the program takes
the else branch of the conditional and emits @racket[O2].

@examples[#:label #f #:eval esterel-eval
          (eval:no-prompt (define-signal S O1 O2))
          (react!
           (esterel
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
@examples[#:label #f #:eval esterel-eval
          (react!
           (esterel
            (emit S)
            (if (present? S)
                (emit O1)
                (emit O2))))]

But Esterel does not require the emission to be sequentially
ordered with the @racket[present?] test. Since Esterel's
notion of parallelism is deterministic, this program is
guaranteed to behave as the previous one, always emitting
@racket[O1] and never emitting @racket[O2], even though the
test for @racket[S] is done in parallel to the emission of
@racket[S].

@examples[#:label #f #:eval esterel-eval
          (react! (esterel
                   (par (emit S)
                        (if (present? S)
                            (emit O1)
                            (emit O2)))))]

To understand this, we must turn to the idea of causality.
That is, instead of a conventional understanding of the two
parallel branches of this @racket[par] as running at the
same time as each other, another way to think of them is
that there is no causality relationship between the two
evaluations and thus Esterel is free to run them however it
wants, subject to other causality dependencies being obeyed.
In other words, to Esterel, the difference between combining
two expression in parallel or sequentially is that combining
them sequentially forces a causality ordering on them, but
combining them in parallel does not.

Even though @racket[par] did not introduce a causal
dependency in this program, there still is a causality
dependency introduced. Specifically, each use of
@racket[emit] introduces a causality dependency between it
and any uses of @racket[present?] (for the same signal). So,
in this program, @racket[(emit S)] @emph{causes}
the call @racket[(present? S)] to return @racket[#true] and
therefore therefore we can take only the branch that emits
@racket[O1] and not the one that emits
@racket[O2].

This notion of causality determining the behavior of the
program also comes with the possibility of a new kind of
error, namely ones with causality loops. Such programs in
Esterel are called ``non constructive'' and @racket[react!]
will raise an error when it encounters one. Here's an
example:

@examples[#:label #f #:eval esterel-eval
          (eval:no-prompt
           (define non-causal
             (esterel
              (if (present? S)
                  (emit S)
                  (emit S)))))
          (eval:error (react! non-causal))]

When looking at this program from a simple logical
perspective, we can see that it makes no sense for
@racket[S] to be absent since that would cause the
@racket[if] to take the else branch which would cause
@racket[S] to be emitted.

But, from a causality perspective, it also makes no sense
for @racket[S] to be present. There is nothing that happens
in the program before the call to @racket[present?] passing
@racket[S] (and thus no emits happen). Thus, from a
causality perspective, since there is nothing that can cause
@racket[S] to be present, @racket[S] must be absent. In
order for this program to be error-free, the
@racket[(emit S)] that happens in the then branch is
effectively causing information flow to go ``backwards'',
against causation, as @racket[present?] cannot decide to
return @racket[#t] without looking into the future beyond
the moment where it is invoked. Accordingly this program is
an error.

@section[#:tag "sec:conts"]{Using Continuations to Replay
 Reactions and How Mutation Causes Trouble}

This section tries to explain, at a high-level how Esterel
in Racket runs code in order to convey an intuition for how
using Racket-level state goes wrong inside a
@racket[esterel].

When a program runs within an @tech{instant}, it runs in two
modes: ``can mode'' and ``must mode''; see
@citet[EsterelConstructiveBook]'s @italic{Constructive
 Semantics of Esterel} for a fuller description of the Can
and Must functions that these modes mimic.

First, the program runs in ``must mode'' where
@racket[present?] does not return @racket[#f], but instead
blocks (@racket[present?] might return @racket[#t] if an
@racket[emit] for the corresponding signal happens). In this
mode, the code is running very much like regular Racket code
runs; internally @racket[par] is creating racket-level
threads and @racket[with-trap] creates an
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{escape
 continuation}.

At some point, however, all of the threads that are still
running get blocked on calls to @racket[present?] where the
corresponding signals have not yet been emitted. At this
point, the program enters ``can mode''. It collects the set
of all of the continuations of all of the threads that are
blocked in this manner and then runs each of them forward
multiple times, once for each possible assignments of absent
and present to each of the signals that's being blocked on.
After this completes, either some of those signals were
never emitted during this process, or the program is
non-constructive. If there were some signals that were never
emitted, they are set to absent and we return to ``must
mode''. If all of the blocking signals were emitted during at
least one of these runs, the program aborts with the
non-constructive error.

Accordingly, if one of the threads performs some mutation,
then that mutation is not rolled back by the continuations,
so it will happen possibly a large number of times and
certainly more than just once.
