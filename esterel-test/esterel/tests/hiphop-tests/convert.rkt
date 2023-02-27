#lang racket
(require esterel/full (for-syntax syntax/parse))
(provide run-hiphop-test)

(define (run-hiphop-test test)
  (match test
    [`(test-case
       ,fn
       (define-esterel-machine ,_ #:inputs (,si ...) #:outputs (,so ...) #:input/outputs (,sio ...) ,expr)
       (test-seq
        ,_
        ,input-outputs ...))
     (define (go signals)
       (define has-pre?
         (let loop ([expr expr]) (match expr [(cons a b) (or (loop a) (loop b))] ['pre& #t] [_ #f])))
       (define r (reaction #:pre (if has-pre? 1 0) (&->dyn expr signals)))
       (let/ec escape
         (for ([input-output (in-list input-outputs)]
               [i (in-naturals)])
           (define-values (s-inputs s-emitteds)
             (match input-output
               [`((,s-inputs ...) => (,s-emitteds ...))
                (values s-inputs s-emitteds)]
               [`((,s-inputs ...) => #:causality-error)
                (values s-inputs 'not-constructive)]))
           (define result
             (with-handlers ([exn:fail:not-constructive?
                              (λ (x) 'not-constructive)])
               (react! r
                       #:emit
                       (for/list ([s-input (in-list s-inputs)])
                         (hash-ref signals s-input)))))
           (define expected-outputs
             (if (equal? s-emitteds 'not-constructive)
                 'not-constructive
                 (for/set ([s-emitted (in-list s-emitteds)])
                   (hash-ref signals s-emitted))))
           (define (output-signal? s)
             (or (for/or ([si (in-list so)])
                   (equal? (hash-ref signals si) s))
                 (for/or ([sio (in-list sio)])
                   (equal? (hash-ref signals sio) s))))
           (define actual-outputs
             (cond
               [(equal? result 'not-constructive)
                'not-constructive]
               [else
                (for/set ([(signal emitted?) (in-hash result)]
                          #:when emitted?
                          #:when (output-signal? signal))
                  signal)]))
           (unless (equal? expected-outputs actual-outputs)
             (eprintf "reaction ~a (counting from 0):\n  file ~a\n  expected ~s\n       got ~s\n"
                      i fn expected-outputs actual-outputs)
             (escape (void))))))
     (with-signals-table signals (hash) (append si so sio)
       ()
       (a b)
       (a b c r o)
       (a b c)
       (a b c d)
       (a b o)
       (a b r o)
       (a b end1 end2)
       (a t v)
       (i o)
       (i j k)
       (i j o)
       (i j k v)
       (i s)
       (j)
       (o)
       (o s)
       (o1 o2)
       (s o f w)
       (s o f w z)
       (s1_and_s2 s1_and_not_s2 not_s1_and_s2 not_s1_and_not_s2)
       (t)
       (t v)
       (ul ur ll lr
           watch_mode_command enter_set_watch_mode_command set_watch_command
           next_watch_time_position_command exit_set_watch_mode_command toggle_24h_mode_command
           toggle_chime_command stopwatch_mode_command start_stop_command
           lap_command alarm_mode_command enter_set_alarm_mode_command
           set_alarm_command next_alarm_time_position_command toggle_alarm_command
           stop_alarm_beep_command exit_set_alarm_mode_command)
       (go signals))]))

#|

The macro `with-signals-table` is a horrible hack.
The way it works is it is prepared for some number of
fixed sets of symbols, for which it has set up an
appropriate use of with-signals. Then, you pass in
some dynamically computed set of signals and it checks
to see if that's one of the ones that it has been set
up for and, if so, it uses it. Otherwise, it raises
an error. This works only because we have a fixed set
of hiphop test cases we want to try to run. But, if
there is a new hiphop test that has a new set of signals
used in it, well, we'll have to add to one of the uses
of this macro ....

|#
(define-syntax (with-signals-table stx)
  (syntax-parse stx
    [(_ signals-table-id:id init-hash the-signals-expr (signal-name:id ...) ... body)
     (define line (syntax-line stx))
     (define src (syntax-source stx))
     #`(let ([signals-names the-signals-expr]
             [init-hash-x init-hash])
         (cond
           [(equal? signals-names '(signal-name ...))
            (let-signal (signal-name ...)
                        (let ([signals-table-id
                               (for/fold ([hash init-hash-x])
                                         ([name (in-list '(signal-name ...))]
                                          [val (in-list (list signal-name ...))])
                                 (hash-set hash name val))])
                          body))] ...
           [else
            (error 'with-signals-table "unknown signals\n  names: ~s\n  srcloc: ~a:~a"
                   signals-names
                   '#,src
                   #,line)]))]))

(define (&->dyn expr signals)
  (let reactor->esterel ([expr expr]
                         [signals signals]
                         [traps (hash)])
    (match expr
      [`(signal& ,(? symbol? s) ,body1 ,body2 ...)
       (reactor->esterel `(signal& (,s) ,body1 ,@body2)
                         signals
                         traps)]
      [`(signal& (,(? symbol? signal-syms) ...) ,body1 ,body2 ...)
       (define (continue signals)
         (reactor->esterel `(seq& ,body1 ,@body2)
                           signals
                           traps))
       (with-signals-table signals signals signal-syms
         (s)
         (s1)
         (s2)
         (s1 s2)
         (l)
         (i)
         (continue signals))]
      [`(seq& ,es ...) (for ([e (in-list es)]) (reactor->esterel e signals traps))]
      [`(loop-each& ,r ,e1 ,e2s ...)
       (loop
        (for ([e (in-list (cons e1 e2s))])
          (reactor->esterel e signals traps))
        #:each (present? (hash-ref signals r)))]
      [`(loop& ,e1 ,e2s ...)
       (let loop-loop ()
         (for ([e (in-list (cons e1 e2s))])
           (reactor->esterel e signals traps))
         (loop-loop))]
      [`(par& ,es ...)
       (let p-loop ([es es])
         (cond
           [(null? (cdr es)) (reactor->esterel (car es) signals traps)]
           [else (par (reactor->esterel (car es) signals traps)
                      (p-loop (cdr es)))]))]
      [`(await& ,s) (await (present? (hash-ref signals s)))]
      [`(await& ,(? natural? n) ,s)
       (for ([i (in-range n)])
         (await (present? (hash-ref signals s))))]
      [`(abort& pre& ,s ,body1 ,body2 ...)
       (abort (reactor->esterel `(seq& ,body1 ,@body2) signals traps)
              #:when (present? (hash-ref signals s) #:pre 1))]
      [`(abort& ,s ,body1 ,body2 ...)
       (abort (reactor->esterel `(seq& ,body1 ,@body2) signals traps)
              #:when (present? (hash-ref signals s)))]
      [`(emit& ,s) (emit (hash-ref signals s))]
      [`(present& ,s ,thn ,els) (if (present? (hash-ref signals s))
                                    (reactor->esterel thn signals traps)
                                    (reactor->esterel els signals traps))]
      [`(present& pre& ,s ,thn ,els) (if (present? (hash-ref signals s) #:pre 1)
                                         (reactor->esterel thn signals traps)
                                         (reactor->esterel els signals traps))]
      [`(suspend& ,s ,body1 ,body2 ...)
       (suspend (reactor->esterel `(seq& ,body1 ,@body2) signals traps)
                (present? (hash-ref signals s)))]
      [`pause& (pause)]
      [`nothing& (void)]
      [`halt& (halt)]
      [`(await-immediate& ,i) (await #:immediate (present? (hash-ref signals i)))]
      [`(every& ,(? symbol? s) ,body) (every (present? (hash-ref signals s)) #:do (reactor->esterel body signals traps))]
      [`(every& (,(? natural? n) ,(? symbol? s)) ,body)
       (every (present? (hash-ref signals s)) #:n n #:do (reactor->esterel body signals traps))]
      [`(every& ,s #:immediate ,body) (every #:immediate (present? (hash-ref signals s)) #:do (reactor->esterel body signals traps))]
      [`(trap& ,t ,body) (with-trap T (reactor->esterel body signals (hash-set traps t T)))]
      [`(exit& ,t) (exit-trap (hash-ref traps t))]
      [`(sustain& ,s) (sustain (hash-ref signals s))]
      [`(weak-abort& ,s ,e1 ,e2 ...)
       (abort #:weak
              (for ([e (in-list (cons e1 e2))])
                (reactor->esterel e signals traps))
              #:when (present? (hash-ref signals s)))]
      [`(weak-abort-immediate& ,s ,e1 ,e2 ...)
       (abort #:weak
              (for ([e (in-list (cons e1 e2))])
                (reactor->esterel e signals traps))
              #:when-immediate (present? (hash-ref signals s)))]
      )))
