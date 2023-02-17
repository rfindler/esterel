#lang scribble/doc
@(require scribble/manual)
@title[#:tag "kernel"]{Kernel Esterel Reference}

@defmodule[esterel/kernel #:no-declare]
@declare-exporting[esterel/kernel esterel]

The @racketmodname[esterel/kernel] and
@racketmodname[esterel] libraries provide all of the names
documented here; the @racketmodname[esterel] library
provides additional functionality.

@(require scribble/example
          (for-label racket/base
                     esterel))
@(define esterel-eval (make-base-eval '(require esterel)))

@section{Reactions}

@defform[(reaction maybe-pre expr ...)
         #:grammar
         ([maybe-pre (code:line) (code:line #:pre pre-count-expr)])]{

 Creates an Esterel reaction that, when passed to
 @racket[react!] will evaluate the @racket[expr]s in a
 context where @racket[in-reaction?] returns @racket[#t].

 If present, the value of @racket[pre-count-expr] is
 expected to be a natural number. It is a limit on the
 history that's saved for signals in previous instants.
 It defaults to @racket[0].
 
}

@defproc[(react! [r reaction?]
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
 start of the reaction; valued signals must be paired with
 values.

 The result has the values of signals that were emitted and,
 if a signal's lack of emission affected the computation, it
 is also included in the resulting hash.

 If the reaction is not constructive, an exception (that is
 recognized by @racket[exn:fail:not-constructive?]) is
 raised.
 
 @examples[
 #:eval esterel-eval
 (define S1 (signal))
 (define S2 (signal))
 (react! (reaction (emit S1)))
 (react! (reaction (if (present? S1) (void) (emit S2))))
 (react! (reaction (if (present? S1) (void) (emit S2)))
         #:emit (list S1))
 (eval:error (react! (reaction (if (present? S1) (emit S1) (void)))))
 ]

}

@defproc[(reaction? [v any/c]) boolean?]{
 Recognizes the result of @racket[reaction].
}

@defproc[(in-reaction?) boolean?]{

 Returns @racket[#t] if it is called in the dynamic extent
 of one of the expressions in a @racket[reaction].
 
}

@defproc[(exn:fail:not-constructive? [v any/c]) boolean?]{

 Recognizes the exception that @racket[react!] raises when
 the reaction is not constructive.
 
}

@section{Signals}

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
 @examples[
 #:eval esterel-eval
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

 @examples[
 #:eval esterel-eval
 (define S (signal))
 (signal-name S)
 (signal-name (signal))]
}

@defproc[(signal? [v any/c]) boolean?]{
 Determines if @racket[v] is a signal, i.e. returned from @racket[signal].
}

@defproc[(signal-combine [s signal?]) (or/c #f (-> any/c any/c any/c))]{
 Returns the combining operation for @racket[s], or @racket[#f] if
 @racket[s] is not a value-carrying signal.
}

@defproc[(present? [s signal?]
                   [#:pre pre natural? 0])
         boolean?]{
 Determines if @racket[s] is present in the current instant when @racket[pre] is @racket[0].

 If @racket[pre] is larger than zero, returns whether or not
 @racket[s] was present in previous instants. If @racket[pre]
 is larger than the value of the @racket[_pre-count-expr]
 passed to @racket[reaction], an error is raised.

 @examples[
 #:eval esterel-eval
 (define S (signal))
 (define O1 (signal))
 (define O2 (signal))
 (react! (reaction (if (present? S) (emit O1) (emit O2))))
 (define r
   (reaction
    #:pre 1
    (emit S)
    (pause)
    (if (present? S #:pre 1) (emit O1) (emit O2))))
 (react! r)
 (react! r)
 ]
 
}

@defproc[(signal-value [s signal?] [#:pre pre natural? 0]) any/c]{
Returns the value of @racket[s] in the current instant if @racket[pre] is @racket[0].

 If @racket[pre] is larger than zero, then returns the value
 of @racket[s] is the @racket[n]th previous instant. If @racket[pre] is
 larger than the value of the @racket[_pre-count-expr] passed to
 @racket[reaction], an error is raised.

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
 (define S1 (signal #:combine +))
 (define r
   (reaction
    (let loop ([n 0])
      (when (even? n)
        (emit S1 n))
      (pause)
      (loop (+ n 1)))))
 (react! r)
 (react! r)
 (react! r)
 (react! r)
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
 (define S1 (signal))
 (define S2 (signal))
 (define r
   (reaction
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
 (react! r)
 (react! r)
 (react! r)
 (react! r)
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
 (define S1 (signal))
 (define S2 (signal))
 (define r
   (reaction
    (with-trap t
      (par (begin
             (emit S1)
             (exit-trap t)
             (emit S2))
           (begin 
             (pause)
             (emit S1))))))
 (react! r)
 (react! r)
 ]
}

@defproc[(exit-trap [t trap?]) any/c]{
 Exits to the trap @racket[t]; does not return. See also @racket[with-trap].
}

@defproc[(trap? [v any/c]) boolean?]{
 Recognizes values bound by @racket[with-trap].
}

@(close-eval esterel-eval)
