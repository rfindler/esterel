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
                     racket/set
                     racket/format
                     esterel/full))
@(define esterel-eval (make-base-eval '(require racket/set esterel/full racket/format)))

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

 The result has the values of signals that were emitted.
 Additionally if a signal is not a valued signal and the
 computation depends on it not being present (e.g., if it is
 passed to @racket[present?]), it is included in resulting
 hash, mapped to @racket[#f].
 
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

@defform[(debug-when-must e1 e2 ...)]{
 Evaluates @racket[e1] and @racket[e2]s when the entire
 @racket[debug-when-must] form must be executed. Raises an error
 if evaluated outside of an @racket[esterel] form.

 This can be used to debug Esterel in Racket programs.
 Sometimes, code inside @racket[esterel] is run as part of an
 exploration to determine if signal might be emitted and, in
 that case, we do not know that that code must run. In such
 situations, effectful code (such as @racket[printf]) can run
 multiple times, leading to confusing behavior. Wrapping such
 debugging IO operations in a @racket[debug-when-must] form can
 help to understand an Esterel in Racket program.


 In this example, the use of @racket[debug-when-must] prevents
 @racket["hi!"] from being printed out, as that code runs
 only in can exploration. In contrast @racket["bye!"] is printed
 out because that code must run, as @racket[_S1] is not present.
 Note that without the @racket[debug-when-must] forms wrapped
 around the calls to @racket[printf], more printouts happen.
 @examples[
 #:eval esterel-eval
 (define-signal S1)
 (react! (esterel (if (present? S1)
                      (debug-when-must (printf "hi!\n"))
                      (debug-when-must (printf"bye!\n")))))
 ]
}

@section{Signals}

@defform[(with-signal (signal ...)
           body-expr ...+)
         #:grammar ([signal
                     (code:line signal-id maybe-combine)]
                    [maybe-combine
                     (code:line)
                     (code:line #:combine combine-expr)
                     (code:line #:init init-expr #:combine combine-expr)
                     #:single])]{
 Creates new signals and binds them to the the @racket[signal-id]s.

 Each signal suffixed with @racket[#:combine] is a value-carrying
 signal, and those without are not. Multiple emissions of the signal are
 combined using the result of @racket[combine-expr], a binary
 function that is assumed to be associative and commutative.
 If @racket[init-expr] is provided, then the value of the signal
 if it is never emitted is the value of @racket[init-expr]. Once
 the signal is emitted, however, the value of @racket[init-expr]
 is discarded.

 If the signal is followed by @racket[#:single], it is also
 a valued signal, but it may be emitted at most once in each
 instant and it takes that value.

 If @racket[with-signal] is invoked from within
 @racket[esterel], then the signals may not be emitted
 once the last @racket[body-expr] is evaluated (it will result in
 an error from @racket[emit] if they are).

 The result of the @racket[with-signal] expression is the
 result of the last expression. If @racket[with-signal] is
 used in the dynamic extent of @racket[esterel], the last
 @racket[body-expr] is not in tail position with respect to
 the @racket[with-signal], but otherwise it is.

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

 (react!
  (esterel
   (with-signal (s1 #:combine +
                 s2 #:init 11 #:combine +
                 s3 #:combine +
                 s4 #:init 22 #:combine *)
     (emit s1 (signal-value s2 #:can (set s3 s4)))
     (emit s4 33)
     (emit s3 (signal-value s4 #:can (set))))))
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

@defproc[(make-global-signal [name string?]
                             [#:combine combine #f (or/c #f (procedure-arity-includes/c 2))]
                             [#:init init any/c])
         signal?]{

 Creates a global signal named @racket[name]. If @racket[combine] is not @racket[#f],
 creates a valued signal. The @racket[init] argument is not required. If it is not
 supplied, then the signal has no initial value, otherwise the initial value is @racket[init].
                                                                                            
 Use @racket[make-global-signal] when the number of signals
 is not known ahead of time or it is not convenient to write
 a sequence of @racket[define-signal] definitions.

 @examples[
 #:eval esterel-eval
 (define sigs
   (for/hash ([i (in-range 10)])
     (values i (make-global-signal (~a i)))))
 sigs
 ]
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

@defproc[(signal-index [s signal?]) (or/c #f natural?)]{
 Returns the index of a signal. This index counts the number
 of times the @racket[with-signal] that introduced @racket[s]
 has been executed to produce this particular signal. If the
 signal was created outside the dynamic extent of @racket[esterel],
 @racket[signal-index] returns @racket[#f].

 @examples[
 #:eval esterel-eval
 (define-signal O)
 (eval:check (signal-index O) #f)
 (define r
   (esterel
    (loop
     (with-signal (S)
       (if (present? S)
           (emit O)
           (void))
       (pause)
       (emit S)))))
 (eval:check
  (for/set ([(k v) (in-hash (react! r))])
    (list (signal-name k) (signal-index k) v))
  (set (list "S" 0 #f)))
 (eval:check
  (for/set ([(k v) (in-hash (react! r))])
    (list (signal-name k) (signal-index k) v))
  (set (list "S" 0 #t)
       (list "S" 1 #f)))
 (eval:check
  (for/set ([(k v) (in-hash (react! r))])
    (list (signal-name k) (signal-index k) v))
  (set (list "S" 1 #t)
       (list "S" 2 #f)))]
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

@defproc[(signal-value [s signal?] [#:pre n natural? 0] [#:can can (setof signal?)]) any/c]{
Returns the value of @racket[s] in the current instant if @racket[n] is @racket[0],
 unless it hasn't been emitted, in which case it returns the value in the previous
 instant.

 If @racket[n] is larger than zero, then @racket[signal-value] returns the value
 of @racket[s] is the @racket[n]th previous instant. If @racket[n] is
 larger than the value of the @racket[_pre-count-expr] passed to
 @racket[esterel], an error is raised. If the value has never been
 emitted and the signals declaration did not have an @racket[#:init]
 clause, an error is raised.

 The @racket[can] argument indicates which signals can be
 emitted by the remaining computation and it must be supplied
 if if @racket[n] is @racket[0]. That is, if it is possible
 that some signal can be emitted in the current instant after
 @racket[signal-value] returns, then that signal must be in
 the set @racket[can].
 
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
    (emit O1 (signal-value S1 #:can (set O2 O3)))
    (emit O2 (signal-value S2 #:pre 1))
    (emit O3 (signal-value S2 #:can (set)))))
 (eval:check (react! r) (hash S1 5 S2 0))
 (eval:check (react! r) (hash O1 5 O2 0 O3 6 S2 6))
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
 them to complete. The result is a set of all of the values
 of the @racket[expr]s.

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
 a true value. The suspensions also affect any
 @racket[with-signal] forms in (the dynamic extent of)
 @racket[when-expr].

 In the second instant in this example, @racket[S1] is not
 emitted because that thread is suspended. In the third
 instant, however it is, because @racket[S2] is not emitted.

 @examples[
 #:eval esterel-eval
 #:label #f
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

 Signals whose declaration are suspended do not see the
 instants that happen during the suspension. Specifically,
 the pre values are not affected by instants that are unseen
 because of the suspension, as this example demonstrates.
 @examples[
 #:eval esterel-eval
 #:label #f
 (define-signal
   O #:init (set) #:combine set-union
   S2)
 (define r
   (esterel
    #:pre 1
    (suspend
     (with-signal (S1)
       (let loop ()
         (emit S1)
         (pause)
         (emit O (present? S1 #:pre 1))
         (loop)))
     (present? S2))))

 (react! r)
 (react! r #:emit (list S2))
 (react! r)
 ]

 In the second instant, @racket[S1] is not emitted but, because its
 declaration is suspended, the present test on the previous
 instant's version of @racket[S1] is true and thus the result
 of @racket[react!] has @racket[O] mapped to @racket[#t].
}

@defform[(with-trap trap-id body-expr ...)]{

 Binds @racket[trap-id] to a newly created trap and evaluates the
 @racket[body-expr]s. If the trap bound to @racket[trap-id] is
 passed to @racket[exit-trap], the computation in the rest of
 the @racket[body-expr]s is skipped and the result of the
 @racket[with-trap] is the value passed to @racket[exit-trap].

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

@defproc[(exit-trap [t trap?] [v any/c (void)]) any/c]{
 Exits to the trap @racket[t] with the value @racket[v]; does not return.

 See also @racket[with-trap].
}

@defproc[(trap? [v any/c]) boolean?]{
 Recognizes values bound by @racket[with-trap].
}

@defform[(exec esterel-id
               ([id id-expr] ...)
               exec-expr ...
               maybe-kill-exprs
               maybe-suspend-exprs
               maybe-resume-exprs)
         #:grammar
         ([maybe-kill-exprs (code:line) (code:line #:kill kill-expr ...)]
          [maybe-suspend-exprs (code:line) (code:line #:suspend suspend-expr ...)]
          [maybe-resume-exprs (code:line) (code:line #:resume resume-expr ...)])]{

 Creates a separate thread to evaluate the @racket[exec-expr]s.
 Meanwhile, @racket[pause]s until the first instant after the
 @racket[exec-expr]s terminate.

 The @racket[id-expr] expressions are evaluated when the
 @racket[exec] is evaluated and they are bound to the
 @racket[id]s, whose scope spans the @racket[exec-expr]s,
 @racket[kill-expr]s, @racket[suspend-expr]s, and the
 @racket[resume-expr]s.

 The @racket[suspend-expr]s and @racket[resume-expr]s are
 evaluated each instant that the @racket[exec] is
 @racket[suspend]ed or resumed, with the caveat that, if the
 @racket[exec] is suspended for multiple instants, the
 @racket[suspend-expr]s are evaluated only during the first
 instant when the @racket[expr] is suspended. Similarly, when
 the @racket[exec] is resumed, the @racket[resume-expr]s are
 evaluated only once, until the next time the @racket[exec]
 is resumed (following some future suspension).
 
 If the @racket[exec] is terminated (because a parallel
 thread @racket[exit-trap]s to an enclosing @racket[with-trap])
 the @racket[kill-expr]s are evaluated.

 If they are evaluated, the @racket[kill-expr]s,
 @racket[suspend-expr]s, and @racket[resume-expr]s are
 evaluated on the same thread as each other, and that thread
 is different from the thread evaluating the @racket[exec-expr]s.
 Also, the @racket[kill-expr]s, @racket[suspend-expr]s, and
 @racket[resume-expr]s must not raise errors or otherwise
 fail to return normally, as the overall @racket[esterel]
 that they are running in will not function properly in that
 situation.

 The intention of @racket[exec] is to offer a controlled way
 to connect the synchronous computation (inside
 @racket[esterel]) to the asynchronous computation that's
 going on outside it. As such, the idea is that the
 @racket[exec-expr]s may run some asynchronous computation,
 such as connecting to a website or waiting for a timeout
 and, when that completes, trigger another reaction with the
 results of the asynchronous computation communicated to the
 synchronous world via values on signals. That is, the last
 thing that the @racket[exec-exprs]s should do is trigger
 a reaction. But, that reaction should probably be triggered
 on the same thread that triggered the original reaction (or,
 at least, in a way that guarantees that only one reaction is
 running at a time). The syncronization required to establish
 that is left to the user of @racket[exec]. One possibility,
 if a GUI is involved, is to use @racket[queue-callback] in
 the @racket[exec-expr]s, keeping every reaction on an
 eventspace's handler thread.

}

@(close-eval esterel-eval)
