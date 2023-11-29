#lang scribble/doc
@(require scribble/manual)
@title[#:tag "reference"]{Esterel Reference}

@defmodule[esterel/full]

The @racketmodname[esterel/full] library provides all of
the names documented here as well as the names documented
in @racketmodname[esterel/kernel].

@(require scribble/example
          (for-label racket/base
                     racket/format
                     racket/set
                     esterel/full))
@(define esterel-eval
   (make-base-eval '(require esterel/full racket/format racket/set)))

@defproc[(halt) any/c]{
 Pauses in every instant, forever.
}

@defproc[(sustain [s signal?]) any/c]{
 Emits @racket[S] an pauses in every instant, forever.

 @examples[
 #:eval esterel-eval
 (define-signal S)
 (define r
   (esterel
    (sustain S)))
 (eval:check (react! r) (hash S #t))
 (eval:check (react! r) (hash S #t))
 (eval:check (react! r) (hash S #t))
 ]
}

@defform*[[(loop body-expr ...+)
           (loop body-expr ...+ #:each test-expr)]]{

In the first form, runs @racket[body-expr] over and over.
In the second form, starts by running @racket[body-expr] and then @racket[halt]ing.
 Restarts @racket[body-expr] when @racket[test-expr] becomes true.

 For example, this program emits @racket[S1] in every instant that @racket[S2] is
 not present.
@examples[
 #:label #f
 #:eval esterel-eval
 (define-signal S1 S2)
 (define r
   (esterel
    (loop (emit S1)
          #:each (not (present? S2)))))

 (eval:check (react! r) (hash S1 #t))
 (eval:check (react! r) (hash S1 #t S2 #f))
 (eval:check (react! r #:emit (list S2)) (hash S2 #t))
 (eval:check (react! r #:emit (list S2)) (hash S2 #t))
 (eval:check (react! r) (hash S1 #t S2 #f))
 (eval:check (react! r) (hash S1 #t S2 #f))
 ]
}

@defform*[[(abort body-expr ...+ #:when when-expr)
           (abort #:weak body-expr ...+ #:when when-expr)
           (abort #:weak body-expr ...+ #:when-immediate when-expr)]]{

 Terminates when @racket[body-expr] terminates or when @racket[when-expr]
 returns a true value. If @racket[#:weak] is present, the
 @racket[body-expr]s are executed when @racket[when-expr] is true; they
 are skipped if @racket[#:weak] is not present. If @racket[#:when] is
 used, the @racket[body-expr]s are executed at least once and the
 @racket[abort] runs for at least one instant; if @racket[#:when-immediate]
 is used, the @racket[when-expr] is tested in the first instant and
 the expression terminates in the first instant if @racket[when-expr]
 returns a true value.

 For example, this repeatedly emits @racket[S1] until
 @racket[S2] is present, in which case the entire loop
 terminates and we fall through to the emit of @racket[S3].
 @examples[
 #:label #f
 #:eval esterel-eval
 (define-signal S1 S2 S3)
 (define r1
   (esterel
    (abort (let loop ()
             (emit S1)
             (pause)
             (loop))
           #:when (present? S2))
    (emit S3)))
 (eval:check (react! r1) (hash S1 #t))
 (eval:check (react! r1) (hash S1 #t S2 #f))
 (eval:check (react! r1) (hash S1 #t S2 #f))
 (eval:check (react! r1) (hash S1 #t S2 #f))
 (eval:check (react! r1 #:emit (list S2)) (hash S3 #t S2 #t))
 ]

 If @racket[S2] had been present in the first instant, that program
 would not have terminated in the first instant, but in the second:

 @examples[
 #:label #f
 #:eval esterel-eval
 (define r2
   (esterel
    (abort (let loop ()
             (emit S1)
             (pause)
             (loop))
           #:when (present? S2))
    (emit S3)))
 (eval:check (react! r2 #:emit (list S2)) (hash S2 #t S1 #t))
 (eval:check (react! r2 #:emit (list S2)) (hash S2 #t S3 #t))
 ]

 To terminate in the first instant, use @racket[#:when-immediate], so that
 the @racket[test-expr] is evaluated in the first instant
 @examples[
 #:label #f
 #:eval esterel-eval
 (define r3
   (esterel
    (abort #:weak
           (let loop ()
             (emit S1)
             (pause)
             (loop))
           #:when-immediate (present? S2))
    (emit S3)))
 (eval:check (react! r3 #:emit (list S2)) (hash S1 #t S2 #t S3 #t))
 ]
 but note that @racket[#:when-immediate] requires @racket[#:weak], so
 @racket[S1] is also emitted.
}

@defform*[[(await when-expr)
           (await when-expr #:n n-expr)
           (await #:immediate when-expr)]]{

 In the first form, @racket[pause]s until @racket[when-expr]
 returns a true value, but at least one instant. In the
 second form, @racket[pause] until @racket[when-expr] returns
 a true value @racket[n-expr] times. In the third form,
 the value of @racket[when-expr] is tested in the first instant,
 and thus the @racket[await] might terminate immediately.

 For example, this program waits two instants before
 emitting @racket[S1]. When that happens, the @racket[await]
 terminates and thus so does the @racket[par], leading to the
 emission of @racket[S2].
 @examples[
 #:label #f
 #:eval esterel-eval
 (define-signal S1 S2)
 (define r
   (esterel
    (par (begin
           (pause)
           (pause)
           (emit S1))
         (await (present? S1)))
    (emit S2)))
 (eval:check (react! r) (hash))
 (eval:check (react! r) (hash S1 #f))
 (eval:check (react! r) (hash S1 #t S2 #t))]

 As an example of the second form, this program emits
 @racket[S2] in the fifth instant; it pauses for three
 instants where @racket[S1] was present and two where it is
 not.

 @examples[
 #:label #f
 #:eval esterel-eval
 (define-signal S1 S2)
 (define r
   (esterel
    (await (present? S1) #:n 3)
    (emit S2)))
 (eval:check (react! r) (hash))
 (eval:check (react! r #:emit (list S1)) (hash S1 #t))
 (eval:check (react! r) (hash S1 #f))
 (eval:check (react! r #:emit (list S1)) (hash S1 #t))
 (eval:check (react! r #:emit (list S1)) (hash S1 #t S2 #t))]

 As an example of the third form, this program emits @racket[O] in the first
 instant, as both of the @racket[await]s terminate in the first @tech{instant}.
 @examples[
 #:label #f
 #:eval esterel-eval
 (define-signal S O)
 (define r (esterel
            (await #:immediate (not (present? S)))
            (await #:immediate (not (present? S)))
            (emit O)))
 (eval:check (react! r) (hash S #f O #t))
 ]

}

@defform*[[(every test-expr #:do body-expr ...+)
           (every test-expr #:n n-expr #:do body-expr ...+)
           (every #:immediate test-expr #:do body-expr ...+)]]{
 In the first form, @racket[await]s @racket[test-expr] evaluating
 to a true value and then starts running the @racket[body-expr]s; when
 whenever @racket[test-expr] becomes true, restarts @racket[body-expr].
 The second form is similar to the first, except that it waits for
 @racket[test-expr] to be true @racket[n-expr] times before restarting
 @racket[body-expr]. In the third form, if @racket[test-expr] evaluates
 to a true value in the current instant, the @racket[body-expr]s are evaluated
 in the current instant.

 For example, this program emits @racket[S2] whenever @racket[S1] is present.
 @examples[
 #:label #f
 #:eval esterel-eval
 (define-signal S1 S2)
 (define r1
   (esterel
    (every (present? S1)
           #:do
           (emit S2))))
 (eval:check (react! r1 #:emit (list S1)) (hash S1 #t))
 (eval:check (react! r1 #:emit (list S1)) (hash S1 #t S2 #t))
 (eval:check (react! r1 #:emit (list S1)) (hash S1 #t S2 #t))
 (eval:check (react! r1) (hash S1 #f))
 (eval:check (react! r1 #:emit (list S1)) (hash S1 #t S2 #t))
 (eval:check (react! r1 #:emit (list S1)) (hash S1 #t S2 #t))
 ]

 Whereas, this program emits @racket[S2] every second time @racket[S1] is present.
 @examples[
 #:label #f
 #:eval esterel-eval
 (define r2
   (esterel
    (every (present? S1)
           #:n 2
           #:do
           (emit S2))))
 (react! r2 #:emit (list S1))
 (react! r2 #:emit (list S1))
 (react! r2 #:emit (list S1))
 (react! r2)
 (react! r2 #:emit (list S1))
 (react! r2 #:emit (list S1))
 ]

 When using @racket[#:immediate], we can emit @racket[S2] in the first instant.
 @examples[
 #:label #f
 #:eval esterel-eval
 (define r2
   (esterel
    (every (present? S1)
           #:n 2
           #:do
           (emit S2))))
 (react! r2 #:emit (list S1))
 (react! r2 #:emit (list S1))
 (react! r2 #:emit (list S1))
 (react! r2)
 (react! r2 #:emit (list S1))
 (react! r2 #:emit (list S1))
 ]
}

@defform[(for/par (for-clause ...) body-or-break ... body)]{
 Just like @racket[for], but combines the iterations of the
 loop with @racket[par].

 Here is an example where we first create 10 signals with
 names from 0 to 9. Then, in parallel, we emit a value on
 each of the signals, where the value on signal n is one more
 than the value on signal n+1, except signal 9 where we
 simply emit a 0. Because all the emissions are happening in
 parallel, we get the values propagating properly from signal
 to signal.

 @examples[
 #:label #f
 #:eval esterel-eval
 (define signal-count 10)
 (define-signals sigs mk-a-signal
   (for/hash ([i (in-range signal-count)])
     (values i (mk-a-signal (~a i) #:combine +))))
 (react!
  (esterel
   (for/par ([(n sig) (in-hash sigs)])
     (cond
       [(= n (- signal-count 1))
        (emit sig 0)]
       [else
        (define n+1-sig
          (hash-ref sigs (+ n 1)))
        (define n+1-value
          (signal-value n+1-sig #:can (set sig)))
        (emit sig (+ n+1-value 1))]))))]
}

@(close-eval esterel-eval)
