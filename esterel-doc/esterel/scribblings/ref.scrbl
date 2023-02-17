#lang scribble/doc
@(require scribble/manual)
@title[#:tag "reference"]{Esterel Reference}

@defmodule[esterel]

The @racketmodname[esterel] library provides all of
the names documented here as well as the names documented
in @racketmodname[esterel/kernel].

@(require scribble/example
          (for-label racket/base
                     esterel))
@(define esterel-eval (make-base-eval '(require esterel)))

@defproc[(halt) any/c]{
 Immediately pauses in every instant.
}


@(close-eval esterel-eval)
