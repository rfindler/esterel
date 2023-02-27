#lang scribble/doc
@(require scribble/manual
          scribble/example
          (for-label racket/base esterel/full))
@(define esterel-eval (make-base-eval '(require esterel/full)))
@(define-syntax-rule (ex expr ...) (examples #:eval esterel-eval #:label #f expr ...))

@title{Esterel in Racket, for Esterel Programmers}

The primary design goal for Esterel in Racket is to
faithfully implement the semantics of Kernel Esterel
(including instantenous reaction to absence) while providing
as seamless an integration with Racket under that constraint
as possible. As such, Esterel in Racket takes on many of the
large scale characteristics of Racket. Probably the largest
one is that there are very few syntactic errors in Esterel
in Racket programs; instead, programs that are erroneous
will raise errors as they execute instead of being ruled out
by a compiler statically. In addition, Esterel has been
pared down to its bare minimum, preferring to use a Racket
construct when it already exists instead of building up a
parallel set of constructs. For example, we simply use
@racket[begin] (or the
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{implicit
 begin} found in many constructs) instead of having an
explicit @tt{seq} construct. And, while there is a
@racket[loop] construct provided by Esterel in Racket, it is
a simple layer over the more conventional Rackety
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{named
 let}.

In order for Racket in Esterel to execute properly, the
programmer must provide an explicit, syntactic delimiter
called @racket[reaction] to indicate where the Esterel
computation starts. This returns a value at the Racket level
which can be passed to @racket[react!] to run a single
instant.

Here's a first example of setting up an Esterel computation
that alternates between emitting a pair of two signals in
each instant:
@ex[
 (eval:no-prompt (define-signal S1 S2))
 (eval:no-prompt
  (define r
    (reaction
     (loop
      (emit S1)
      (pause)
      (emit S2)
      (pause)))))]

Signals do not need to be created from inside the reaction,
but any emission of the signal or other use of it must
happen within. To run an instant, we can simply pass
@racket[r] to @racket[react!], which returns a table that
maps the signals to their status.

@ex[
 (eval:check (react! r) (hash S1 #t))
 (eval:check (react! r) (hash S2 #t))
 (eval:check (react! r) (hash S1 #t))
 ]

Note that absent signals are not present in this example;
sometimes they are present, however, when their absence was
required to take a particular conditional branch.

To abstract over repeated patterns, we simply define Racket
functions. As an example, consider the function
@racket[Aux], that demonstrates a few more features of
Esterel in Racket:

@ex[
 (eval:no-prompt
  (define (Aux I O)
    (every #:immediate (present? I)
           #:do
           (pause)
           (emit O))))
 ]

The function @racket[Aux] consumes two parameters, named
@racket[I] and @racket[O], that are meant to be signals and
thus it must be called from within the dynamic extent of
@racket[reaction].

If it is called outside of @racket[reaction] there is no
static checking but an error will be raised because
@racket[present?] checks to make sure it is called dynamically
within a @racket[reaction]:

@ex[
    (eval:error (Aux S1 S2))
 ]

So, we can define a reaction and then call @racket[Aux] from it,
even from two separate threads:

@ex[
 (eval:no-prompt
  (define r
    (reaction
     (emit S1)
     (par (Aux S1 S2)
          (Aux S2 S1)))))
 ]

Like the previous reaction example, alternates between
emitting @racket[S1] and @racket[S2]

@ex[
 (eval:check (react! r) (hash S1 #t S2 #f))
 (eval:check (react! r) (hash S1 #f S2 #t))
 (eval:check (react! r) (hash S1 #t S2 #f))
 ]

This time, however, we see the unemitted signal in the
result of @racket[react!], because the program observed the
absence of the signals via the @racket[present?] function.

