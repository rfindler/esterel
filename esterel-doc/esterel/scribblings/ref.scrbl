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
 Pauses in every instant, forever.
}

@defform[(loop-each body-expr test-expr)]{

Starts by running @racket[body-expr] and then @racket[(halt)]ing.
 Restarts @racket[body-expr] when @racket[test-expr] becomes true.

 For example, this program emits @racket[S1] in every instant that @racket[S2] is
 not present.
@examples[
 #:label #f
 #:eval esterel-eval
 (define S1 (signal))
 (define S2 (signal))
 (define r
   (reaction
    (loop-each
     (emit S1)
     (not (present? S2)))))

 (react! r)
 (react! r)
 (react! r #:emit (list S2))
 (react! r #:emit (list S2))
 (react! r)
 (react! r)
 ]
}

@defform[(abort-when body-expr when-expr)]{

 Terminates when @racket[body-expr] terminates or, after the
 first instant in @racket[body-expr], aborts when
 @racket[when-expr] returns a true value.

 For example, this repeatedly emits @racket[S1] until
 @racket[S2] is present, in which case the entire loop
 terminates and we fall through to the emit of @racket[S3].
 @examples[
 #:label #f
 #:eval esterel-eval
 (define S1 (signal))
 (define S2 (signal))
 (define S3 (signal))
 (define r
   (reaction
    (abort-when (let loop ()
                  (emit S1)
                  (pause)
                  (loop))
                (present? S2))
    (emit S3)))
 (react! r)
 (react! r)
 (react! r)
 (react! r)
 (react! r #:emit (list S2))
 ]
}

@defform[(await for-expr)]{
 Pauses until @racket[for-expr] returns a true value, but at least one instant.

 For example, this program waits two instants before
 emitting @racket[S1]. When that happens, the @racket[await]
 terminates and thus so does the @racket[par], leading to the
 emission of @racket[S2].
 @examples[
 #:label #f
 #:eval esterel-eval
 (define S1 (signal))
 (define S2 (signal))
 (define r
   (reaction
    (par (begin
           (pause)
           (pause)
           (emit S1))
         (await (present? S1)))
    (emit S2)))
 (react! r)
 (react! r)
 (react! r)]
}

@defform[(await-n when-expr n-expr)]{

 Pauses until @racket[n-expr] instants have passed where
 @racket[when-expr] evaluated to a true value.

For example, this program emits @racket[S2] in the fifth instants; it pauses
 for three instants where @racket[S1] was present and two where it is not.
@examples[
 #:label #f
 #:eval esterel-eval
 (define S1 (signal))
 (define S2 (signal))
 (define r
   (reaction
    (await-n (present? S1) 3)
    (emit S2)))
 (react! r)
 (react! r #:emit (list S1))
 (react! r)
 (react! r #:emit (list S1))
 (react! r #:emit (list S1))]

}

@(close-eval esterel-eval)
