#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval)

@title{Esterel in Racket}

@author["Robert Bruce Findler"]

This manual describes an implementation of Esterel that
cooperates with Racket to the best of its ability. In
particular, any side-effect free code should be able to be
mixed with Esterel code and run from within a reaction.

@table-of-contents[]

@include-section["scribblings/overview.scrbl"]
@include-section["scribblings/kernel.scrbl"]
@include-section["scribblings/ref.scrbl"]

@index-section[]


@; Needs a timeout for testing:
@(module* test racket/base
   (require (submod ".."))
   (module config info
     (define timeout 300)))
