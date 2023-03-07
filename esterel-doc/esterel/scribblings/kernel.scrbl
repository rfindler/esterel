#lang scribble/doc
@(require scribble/manual)
@title[#:tag "kernel"]{Kernel Esterel Reference}

@defmodule[esterel/kernel #:no-declare]
@declare-exporting[esterel/kernel esterel/full]

The @racketmodname[esterel/kernel] and
@racketmodname[esterel/full] libraries provide all of the names
documented here; the @racketmodname[esterel/full] library
provides additional functionality.

@(require scribble/example
          (for-label racket/base
                     esterel/full))
@(define esterel-eval (make-base-eval '(require esterel/full)))

@section{Running Esterel Code}

@defform[(esterel maybe-pre expr ...)
         #:grammar
         ([maybe-pre (code:line) (code:line #:pre pre-count-expr)])]{

 Returns a value that, when passed to
 @racket[react!], will evaluate the @racket[expr]s in a
 context where @racket[in-esterel?] returns @racket[#t].

 If present, the value of @racket[pre-count-expr] is
 expected to be a natural number. It is a limit on the
 history that's saved for signals in previous instants.
 It defaults to @racket[0].

}

@defproc[(react! [r esterel?]
                 [#:emit signals
                  (listof
                   (or/c (and/c signal? (not/c signal-combine))
                         (cons/c (and/c signal? signal-combine)
                                 any/c)))
                  '()])
         (hash/dc [s signal?]
                  [v (s) (if (signal-combine s)
                             any/c
                             boolean?)]
                  #:immutable #t #:kind 'flat)]{
 Runs one instant of @racket[r].

 If @racket[signals] are supplied, they are emitted at the
 start of the @tech{instant}; valued signals must be paired with
 values.

 The result has the values of signals that were emitted and,
 if a signal's lack of emission affected the computation, it
 is also included in the resulting hash.

 If the code is not constructive, an exception (that is
 recognized by @racket[exn:fail:not-constructive?]) is
 raised.

 @examples[
 #:eval esterel-eval
 (define-signal S1 S2)
 (react! (esterel (emit S1)))
 (react! (esterel (if (present? S1) (void) (emit S2))))
 (react! (esterel (if (present? S1) (void) (emit S2)))
         #:emit (list S1))
 (eval:error (react! (esterel (if (present? S1) (emit S1) (void)))))
 ]

}

@defproc[(esterel? [v any/c]) boolean?]{
 Recognizes the result of @racket[esterel].
}

@defproc[(in-esterel?) boolean?]{

 Returns @racket[#t] if it is called in the dynamic extent
 of one of the expressions in a @racket[esterel].

}

@defproc[(exn:fail:not-constructive? [v any/c]) boolean?]{

 Recognizes the exception that @racket[react!] raises when an
 @tech{instant} is not constructive.

}

@section{Signals}

@defform[(with-signal (signal ...)
           body-expr ...+)
         #:grammar ([signal
                     (code:line signal-id maybe-combine)]
                    [maybe-combine
                     (code:line)
                     (code:line #:combine combine-expr)])]{
 Creates new signals and binds them to the the @racket[signal-id]s.

 Each signal suffixed with @racket[#:combine] is a value-carrying
 signal, and those without are not. Multiple emissions of the signal are
 combined using the result of @racket[combine-expr], a binary
 function that is assumed to be associative and commutative.

 The result of the @racket[with-signal] expression is the
 result of the last expression. If @racket[with-signal] is
 used in the dynamic extent of @racket[esterel], the last
 @racket[body-expr] is not in tail position with respect to
 the @racket[with-signal], but otherwise it is.

 Also, if @racket[with-signal] is invoked from within
 @racket[esterel], then the signals may not be emitted
 once the last @racket[body-expr] is evaluated (it will result in
 an error from @racket[emit] if they are).

 @examples[
 #:eval esterel-eval
 (react!
  (esterel
   (with-signal (s1 s2)
     (unless (present? s2)
       (emit s1)))))

 (react!
  (esterel
   (with-signal (s1 s2 #:combine + s3 s4 #:combine *)
     (emit s1)
     (emit s2 22)
     (emit s2 33))))
 ]
 
}

@defform[(define-signal signal ...)]{
 Creates signals and binds them to the @racket[_signal-id]s
 in each @racket[signal].

 The signals that @racket[define-signal] creates have
 indefinite extent (i.e., the signal will not become dead
 unlike the signals created by @racket[with-signal]),
 but @racket[define-signal] can be
 used only at the module top-level or at the interactive
 top-level.
}

@defproc[(signal? [v any/c]) boolean?]{
 Determines if @racket[v] is a signal, i.e. returned from @racket[signal].
 @examples[
 #:eval esterel-eval
 (with-signal (s1)
   (signal? s1))
 (signal? "not a signal, but a string")
 ]
}

@defproc[(signal-name [s signal?]) (and/c string? immutable?)]{
 Returns the name of a signal.

 @examples[
 #:eval esterel-eval
 (define-signal S)
 (eval:check (signal-name S) "S")]
}

@defproc[(signal-combine [s signal?]) (or/c #f (-> any/c any/c any/c))]{
 Returns the combining operation for @racket[s], or @racket[#f] if
 @racket[s] is not a value-carrying signal.

 @examples[
 #:eval esterel-eval
 (with-signal (s1 s2 #:combine +)
   (values (signal-combine s1)
           (signal-combine s2)))
 ]
}

@defproc[(present? [s signal?]
                   [#:pre pre natural? 0])
         boolean?]{
 Determines if @racket[s] is present in the current instant when @racket[pre] is @racket[0].

 If @racket[pre] is larger than zero, returns whether or not
 @racket[s] was present in previous instants. If @racket[pre]
 is larger than the value of the @racket[_pre-count-expr]
 passed to @racket[esterel], an error is raised.

 @examples[
 #:eval esterel-eval
 (define-signal S)
 (define-signal O1)
 (define-signal O2)
 (react! (esterel (if (present? S) (emit O1) (emit O2))))
 (define r
   (esterel
    #:pre 1
    (emit S)
    (pause)
    (if (present? S #:pre 1) (emit O1) (emit O2))))
 (react! r)
 (react! r)
 ]
}

@defproc[(signal-value [s signal?] [#:pre n natural? 0]) any/c]{
Returns the value of @racket[s] in the current instant if @racket[n] is @racket[0],
 unless it hasn't been emitted, in which case it returns the value in the previous
 instant.

 If @racket[n] is larger than zero, then returns the value
 of @racket[s] is the @racket[n]th previous instant. If @racket[n] is
 larger than the value of the @racket[_pre-count-expr] passed to
 @racket[esterel], an error is raised.

 @examples[
 #:eval esterel-eval
 (define-signal
   S1 #:combine + S2 #:combine +
   O1 #:combine + O2 #:combine + O3 #:combine +)
 (define r
   (esterel
    #:pre 1
    (emit S1 2)
    (emit S1 3)
    (emit S2 0)
    (pause)
    (emit S2 6)
    (emit O1 (signal-value S1))
    (emit O2 (signal-value S2 #:pre 1))
    (emit O3 (signal-value S2))))
 (eval:check (react! r) (hash S1 5 S2 0))
 (eval:check (react! r) (hash O1 5 O2 0 O3 6 S1 #f S2 6))
 ]

}

@defproc*[([(emit [s (signal?)]) void?]
           [(emit [s (signal?)] [v any/c]) void?])]{

 Emits @racket[s]. If one argument is passed, then @racket[s]
 must not be a value-carrying signal. If two arguments are
 passed then @racket[s] must be a value-carrying signal and
 the value @racket[v] is emitted.

}

@section{Control Operations}

@defform[(par expr ...)]{

 Executes each @racket[expr] in parallel, waiting for all of
 them to complete.

}

@defproc[(pause) void?]{

 Pauses the current thread; when all of the threads are
 paused (or they are canceled via @racket[exit-trap], the
 instant is over. During the next instant, control picks up
 wherever the pause was.


 In this example, we pause in the middle of the loop and
 the value of @racket[n] is carried forward from instant to instant.
 @examples[
 #:eval esterel-eval
 (define-signal S1 #:combine +)
 (define r
   (esterel
    (let loop ([n 0])
      (when (even? n)
        (emit S1 n))
      (pause)
      (loop (+ n 1)))))
 (eval:check (for/list ([i (in-range 5)])
               (react! r))
             (list (hash S1 0)
                   (hash)
                   (hash S1 2)
                   (hash)
                   (hash S1 4)))
 ]

}

@defform[(suspend body-expr when-expr)]{

 When resuming from a @racket[pause] in @racket[body-expr],
 suspends @racket[body-expr] when @racket[when-expr] returns
 a true value.

 In the second instant in this example, @racket[S1] is not
 emitted because that thread is suspended. In the third
 instant, however it is, because @racket[S2] is not emitted.

 @examples[
 #:eval esterel-eval
 (define-signal S1 S2)
 (define r
   (esterel
    (par
     (begin (pause)
            (emit S2)
            (pause))
     (suspend
      (let loop ()
        (pause)
        (emit S1)
        (loop))
      (present? S2)))))
 (eval:check (react! r) (hash))
 (eval:check (react! r) (hash S2 #t))
 (eval:check (react! r) (hash S1 #t S2 #f))
 (eval:check (react! r) (hash S1 #t S2 #f))
 ]
}

@defform[(with-trap trap-id body-expr ...)]{

 Binds @racket[trap-id] to a newly created trap and evaluates the
 @racket[body-expr]s.

 In this example, the @racket[(exit-trap t)] causes the first
 instant to not have @racket[S2] emitted and causes
 the second instant to not have @racket[S1] emitted.
 @examples[
 #:eval esterel-eval
 (define-signal S1 S2)
 (define r
   (esterel
    (with-trap t
      (par (begin
             (emit S1)
             (exit-trap t)
             (emit S2))
           (begin
             (pause)
             (emit S1))))))
 (eval:check (react! r) (hash S1 #t))
 (eval:check (react! r) (hash))
 ]
}

@defproc[(exit-trap [t trap?]) any/c]{
 Exits to the trap @racket[t]; does not return. See also @racket[with-trap].
}

@defproc[(trap? [v any/c]) boolean?]{
 Recognizes values bound by @racket[with-trap].
}

@(close-eval esterel-eval)
