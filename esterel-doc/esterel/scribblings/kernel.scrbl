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
