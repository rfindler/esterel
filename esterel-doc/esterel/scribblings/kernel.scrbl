#lang scribble/doc
@(require scribble/manual)
@title[#:tag "kernel"]{Kernel Esterel Reference}

@defmodule[esterel/kernel #;#:no-declare]
@;@declare-exporting[esterel/kernel esterel]

The @racketmodname[esterel/kernel] library provides all of
the names documented here; see also @racketmod[esterel].

@(require scribble/example
          (for-label racket/base
                     esterel))
@(define esterel-eval (make-base-eval '(require esterel)))

@defform[(reaction expr ...)]{
 Creates an esterel reaction that, when passed to @racket[react!] will
 evaluate the @racket[expr]s.
}

@defproc[(react! [r reaction?]) any/c]{
 Runs one instant of @racket[r].
}

@defproc[(reaction? [v any/c]) boolean?]{
 Recognizes the result of @racket[reaction].
}

@defform[(signal maybe-name maybe-combine)
         #:grammar ([maybe-name
                     (code:line)
                     (code:line #:name name-expr)]
                    [maybe-combine
                     (code:line)
                     (code:line #:combine combine-expr)])]{
Creates a new signal.

 The signal's name is the result of @racket[name-expr] or,
 if @racket[name-expr] is not supplied, and one cannot be inferred
 from the context, it has no name.
 @examples[#:eval esterel-eval
           (define S1 (signal))
           (signal-name S1)

           (signal-name (signal))

           (signal-name (signal #:name "S2"))]

 If @racket[#:combine] is supplied, this is value-carrying
 signal, otherwise not. Multiple emissions of the signal are
 combined using the result of @racket[combine-expr], a binary
 function that is assumed to be associative and commutative.
 
}

@defproc[(signal-name [s signal?]) (or/c #f string?)]{
 Returns the name of a signal.
}

@defproc[(signal? [v any/c]) boolean?]{
 Determines if @racket[v] is a signal, i.e. returned from @racket[signal].
}

@defproc[(signal-combine [s signal?]) (or/c #f (-> any/c any/c any/c))]{
 Returns the combining operation for @racket[s], or @racket[#f] if
 @racket[s] is not a value-carrying signal.
}

@(close-eval esterel-eval)