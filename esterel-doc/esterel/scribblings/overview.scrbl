#lang scribble/doc
@(require scribble/manual
          scribble/example
          (for-label racket/base esterel))
@(define esterel-eval (make-base-eval '(require esterel)))

@title{Esterel Semantics Overview}

Esterel is a synchronous reactive programming language. It
has imperative features and parallelism, but is
deterministic, with every program guaranteed to produce its
particular result no matter how thread interleaving happens.
In Esterel, a program consists of a series of instantaneous
@tech{reactions} where no time passes and, instead a weaker
notion of time, called causality, affects the flow of the
computation.

To start an Esterel computation, one must first create a
@tech{reaction}:
@examples[#:label #f #:eval esterel-eval
          (define S (signal))
          (define r1 (reaction
                      (emit S)))]

This reaction, when it is started, will create a signal and
then emit the signal. Signals in Esterel are the way that a
program can communicate with itself and produce a result for
the outside world, to be examined after the instant is over.
In their simplest form, a signal is either present or
absent. If the signal is passed to @racket[emit], it is
present, and if it is not passed to @racket[emit], it is
absent.

So, we can start the reaction by passing @racket[r1] to
@racket[react!]:

@examples[#:label #f #:eval esterel-eval
          (react! r1)]

The result of @racket[react!] tells us all of the emitted
signals and, if some signal was not emitted and the fact
that it wasn't emitted affected the computation, the result
of @racket[react!] also indicate that.

Esterel programs can also depend on which signals were emitted. In this
example, @racket[_S] is emitted, which triggers @racket[O1] being emitted.
@examples[#:label #f #:eval esterel-eval
          (define O1 (signal))
          (define O2 (signal))
          (react! (reaction
                   (emit S)
                   (if (present? S)
                       (emit O1)
                       (emit O2))))]

This time, @racket[S] is not emitted, so @racket[O2] is
emitted and @racket[O1] is not.
@examples[#:label #f #:eval esterel-eval
          (react! (reaction
                   (if (present? S)
                       (emit O1)
                       (emit O2))))]

Esterel has a construct for parallelism called @racket[par] and, because
Esterel's notion of parallelism is deterministic, this program
is guaranteed to behave as the earlier one that emitted @racket[_S],
always emitting @racket[_O1] and never emitting @racket[_O2], even
though the test for @racket[_S] is done in parallel to the emission of
@racket[_S].
@examples[#:label #f #:eval esterel-eval
          (define O1 (signal))
          (define O2 (signal))
          (react! (reaction
                   (par (emit S)
                        (if (present? S)
                            (emit O1)
                            (emit O2)))))]

