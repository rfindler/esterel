#lang racket
(require "../esterel.rkt")
(define (run-hiphop-test test)
  (match test
    [`(test-case
       ,fn
       (define-esterel-machine ,_ #:inputs (,si ...) #:outputs (,so ...) #:input/outputs (,sio ...) ,expr)
       (test-seq
        ,_
        ,input-outputs ...))
     (define signals
       (for/hash ([name (in-list (append si so sio))])
         (values name (signal #:name name))))
     (define r (build-reaction signals expr))
     (let/ec escape
       (for ([input-output (in-list input-outputs)]
             [i (in-naturals)])
         (match-define `((,s-inputs ...) => (,s-emitteds ...))
           input-output)
         (define result
           (react! r
                   #:emit
                   (for/list ([s-input (in-list s-inputs)])
                     (hash-ref signals s-input))))
         (define expected-outputs
           (for/set ([s-emitted (in-list s-emitteds)])
             (hash-ref signals s-emitted)))
         (define (output-signal? s)
           (or (for/or ([si (in-list so)])
                 (equal? (hash-ref signals si) s))
               (for/or ([sio (in-list sio)])
                 (equal? (hash-ref signals sio) s))))
         (define actual-outputs
           (for/set ([(signal emitted?) (in-hash result)]
                     #:when emitted?
                     #:when (output-signal? signal))
             signal))
         (unless (equal? expected-outputs actual-outputs)
           (eprintf "reaction ~a (counting from 0):\n  file ~a\n  expected ~s\n       got ~s\n"
                    i fn expected-outputs actual-outputs)
           (escape (void)))))]))

(define (build-reaction signals expr)
  (reaction
   (let loop ([expr expr]
              [signals signals]
              [traps (hash)])
     (match expr
       [`(signal& ,s ,body1 ,body2 ...)
        (loop `(seq& ,body1 ,@body2)
              (hash-set signals s (signal #:name s))
              traps)]
       [`(seq& ,es ...) (for ([e (in-list es)]) (loop e signals traps))]
       [`(loop-each& ,r ,e1 ,e2s ...)
        (loop-each
         (for ([e (in-list (cons e1 e2s))])
           (loop e signals traps))
         (hash-ref signals r))]
       [`(loop& ,e1 ,e2s ...)
        (let loop-loop ()
          (for ([e (in-list (cons e1 e2s))])
            (loop e signals traps))
          (loop-loop))]
       [`(par& ,es ...)
        (let p-loop ([es es])
          (cond
            [(null? (cdr es)) (loop (car es) signals traps)]
            [else (par (loop (car es) signals traps)
                       (p-loop (cdr es)))]))]
       [`(await& ,s) (await (hash-ref signals s))]
       [`(await& ,(? natural? n) ,s)
        (for ([i (in-range n)])
          (await (hash-ref signals s)))]
       [`(abort& ,s ,body1 ,body2 ...)
        (abort-when (loop `(seq& ,body1 ,@body2) signals traps)
                    (hash-ref signals s))]
       [`(emit& ,s) (emit (hash-ref signals s))]
       [`(present& ,s ,thn ,els) (if (signal-value (hash-ref signals s))
                                     (loop thn signals traps)
                                     (loop els signals traps))]
       [`(suspend& ,s ,body1 ,body2 ...)
        (suspend (loop `(seq& ,body1 ,@body2) signals traps)
                 (signal-value (hash-ref signals s)))]
       [`pause& (pause)]
       [`nothing& (void)]
       [`halt& (halt)]
       [`(await-immediate& ,i) (await-immediate (hash-ref signals i))]
       [`(every& ,(? symbol? s) ,body) (every (hash-ref signals s) (loop body signals traps))]
       [`(every& ,s #:immediate ,body) (every-immediate (hash-ref signals s) (loop body signals traps))]
       [`(trap& ,t ,body) (with-trap T (loop body signals (hash-set traps t T)))]
       [`(exit& ,t) (exit-trap (hash-ref traps t))]
       [`(sustain& ,s) (sustain (hash-ref signals s))]
       [`(weak-abort& ,s ,e1 ,e2 ...)
        (weak-abort (hash-ref signals s)
                    (for ([e (in-list (cons e1 e2))])
                      (loop e signals traps)))]
       [`(weak-abort-immediate& ,s ,e1 ,e2 ...)
        (weak-abort-immediate
         (hash-ref signals s)
         (for ([e (in-list (cons e1 e2))])
           (loop e signals traps)))]
       ))))

(module+ main
  (require "parse.rkt" "find.rkt")
  (for ([test (in-list (find-all-hiphop-tests))])
    (printf "running ~a\n" test)
    (with-handlers ([exn:fail? (λ (x)
                                 ((error-display-handler)
                                  (exn-message x)
                                  x))])
      (run-hiphop-test
       (load-hiphop-test test)))))
