#lang scribble/doc
@(require scribble/manual
          scribble/example
          scriblib/footnote
          (for-label racket/base esterel/full))
@(define esterel-eval (make-base-eval '(require esterel/full)))
@(define-syntax-rule (ex expr ...) (examples #:eval esterel-eval #:label #f expr ...))

@title{Esterel in Racket, for Esterel Programmers}

The primary design goal for Esterel in Racket is to
faithfully implement the semantics of Kernel Esterel
(including instantaneous reaction to absence) while
providing as seamless an integration with Racket under that
constraint as possible. As such, Esterel in Racket takes on
many of the large scale characteristics of Racket. This
section details those design decisions and how they play out
when using Esterel in Racket.

Each section contains running examples that are meant to be
played around with. The simplest way to do so is to use
DrRacket, which comes with @link["https://racket-lang.org"]{the
 default Racket download}.

@section{A First Esterel in Racket Program}

As a first example, here's ABRO: simply paste the following
code into DrRacket's definitions window and hit the
@onscreen{Run} button to see a few instants run.

@codeblock{
#lang racket
(require esterel/full)

(define-signal A B R O)

(define abro
  (reaction
   (loop (par (await (present? A))
              (await (present? B)))
         (emit O)
         #:each (present? R))))

(react! abro)
(react! abro #:emit (list A B))
(react! abro #:emit (list A))
(react! abro #:emit (list R))
}

The first line declares the language and the second line
loads the Esterel in Racket implementation. Each Esterel
program has to be encapsulated inside @racket[reaction];
this is a dynamic restriction, not a static one. Code inside
@racket[reaction] can call arbitrary helper functions and,
indeed, things like @racket[loop] and @racket[emit] are
defined as Racket macros and functions inside the Racket
library @racketmodname[esterel/full].

Code inside @racket[reaction] doesn't run when the
@racket[reaction] form is evaluated, however. Instead, it
returns a value that encapsulates the Esterel computation.
When that value is passed to the function @racket[react!], a
single instant is run and the values of signals are returned
via a table that maps signals to booleans (if they are not
valued signals) or to their values (if they are).

@section{Using Racket Constructs When Possible}

In general, Esterel has been pared
down, preferring to use a Racket
construct when it already exists instead of building up a
parallel set of constructs for Esterel programs. For example, we simply use
@racket[begin] (or the
@tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{implicit
 begin} found in many constructs) instead of having an
explicit @tt{seq} construct. And, while there is a
@racket[loop] construct provided by Esterel in Racket, it is
a simple layer over the more conventional Racket
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{named
 let} that defines a recursive function.

So, to abstract over repeated patterns in Esterel code, we
simply define Racket functions. As an example, consider the
function @racket[Aux], that demonstrates a few more features
of Esterel in Racket, using @racket[every] with with the
keywords @racket[#:immedate] and @racket[#:do].

@ex[
 (eval:no-prompt
  (define (Aux I O)
    (every #:immediate (present? I)
           #:do
           (pause)
           (emit O))))
 ]

The function @racket[Aux] is an ordinary Racket function and
it consumes two parameters, named @racket[I] and @racket[O].
The parameters are meant to be signals and thus it must be
called from within the dynamic extent of @racket[reaction].

Here is some code that defines two signals and then, in
parallel, invokes @racket[Aux] twice; once with @racket[I]
as @racket[S1] and @racket[O] as @racket[S2] and once the
other way around.

@ex[
 (eval:no-prompt
  (define-signal S1 S2))
 (eval:no-prompt
  (define r
    (reaction
     (emit S1)
     (par (Aux S1 S2)
          (Aux S2 S1)))))
 (react! r)
 (react! r)
 (react! r)
 ]

As you can see, this Esterel program alternates between
emitting @racket[S1] and @racket[S2], instant by instant.

@section{Runtime Errors}

There@note{Racket does have a sister language Typed Racket
 (see @other-doc['(lib "typed-racket/scribblings/ts-reference.scrbl")
                 #:indirect "Typed Racket"])
 that could be used to syntactically rule out certain misuses
 of Esterel in Racket operations; so far, we have
 not investigated this but hope to eventually. } are very few syntactic errors in
Esterel in Racket programs; instead, programs that are
erroneous will raise errors as they execute instead of being
ruled out by a compiler statically.

As one example of a runtime error, here is what happens when
@racket[Aux] is called from outside of @racket[reaction]:

@ex[
    (eval:error (Aux S1 S2))
 ]

This is a dynamic check that happens inside
@racket[present?], and several other Esterel functions. They
check if they are being used in the dynamic extent of
@racket[reaction] and, if not, raise an error. The function
@racket[in-reaction?] returns @racket[#t] when code is
currently executing inside a reaction and @racket[#f]
otherwise.

@section{Signals as Values}

Signals are simply values at the Racket level and can be
passed to and returned from Racket functions, as other
Racket values can. This means that a signal is not equated
with a boolean indicating that it is present or absent, as
it is in normal Esterel code. Instead, the signal has to be
passed to @racket[present?], which returns a Racket boolean
that indicates its presence of absence; thus we have the
opportunity to use boolean-accepting Racket combination
operations (like @racket[and] and @racket[or], etc) before
passing the boolean on to operations like @racket[suspend].

Still, in order to correctly identify nonconstructive
programs, signals must be associated with a specific scope,
disallowing emits after the lexical environment in which a
signal is declared has been exited.

Because we're in the larger context of Racket, we do not
rule out this kind of program statically. Indeed, it is fine
for a signal to leave the lexical environment where it is
created, as long as it does not leave the dynamic extent of
the @racket[let-signal]. For example, we may wish to pass it
to a helper function, as in the example above when we used
@tt{Aux}.

More generally, a program where signals flow around the
program entering and leaving other functions and being
stored in data structures is all be fine, as long as the
@racket[let-signal] form does not terminate before the
signal is emitted.

Thus, we have a dynamic check associated with @racket[emit]
that signals an error when a signal has outlived the dynamic
extent of its creation. Here's an example. The signal
@racket[s1] is created by the @racket[let-signal] form and
then is returned from it and ends up being bound to the
identifier @racket[a-signal-outside-its-extent].

@ex[
    (eval:error
     (react!
      (reaction
       (define a-signal-outside-its-extent
         (let-signal (s1)
           s1))
       (emit a-signal-outside-its-extent))))

    ]

At this point, since the @racket[let-signal] form has
exited, Can should be able to know that the signal is not
going to be emitted. To ensure that that deduction that Can
makes is legal, @racket[emit] raises an error instead of
emitting the signal.