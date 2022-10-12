#lang racket
(require "../esterel.rkt")
(define (run-hiphop-test test)
  (match test
    [`(test-case
       ,fn
       (define-esterel-machine ,_ #:inputs (,si ...) #:outputs (,so ...) #:input/outputs (,s ...) ,expr)
       (test-seq
        ,_
        ,input-outputs ...))
     (define signals
       (for/hash ([name (in-list (append si so s))])
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
         (define (input-signal? s)
           (for/or ([si (in-list si)])
             (equal? (hash-ref signals si) s)))
         (define actual-outputs
           (for/set ([(signal emitted?) (in-hash result)]
                     #:when emitted?
                     #:unless (input-signal? signal))
             signal))
         (unless (equal? expected-outputs actual-outputs)
           (eprintf "reaction ~a (counting from 0):\n  file ~a\n  expected ~s\n       got ~s\n"
                    i fn expected-outputs actual-outputs)
           (escape (void)))))]))

(define (build-reaction signals expr)
  (reaction
   (let loop ([expr expr])
     (match expr
       [`(seq& ,es ...) (for ([e (in-list es)]) (loop e))]
       [`(loop-each& ,r ,e1 ,e2s ...)
        (loop-each
         (for ([e (in-list (cons e1 e2s))])
           (loop e))
         (hash-ref signals r))]
       [`(par& ,es ...)
        (let p-loop ([es es])
          (cond
            [(null? (cdr es)) (loop (car es))]
            [else (par (loop (car es))
                       (p-loop (cdr es)))]))]
       [`(await& ,s) (await (hash-ref signals s))]
       [`(emit& ,s) (emit (hash-ref signals s))]))))
           

(module+ main
  (require "parse.rkt" "find.rkt")
  (run-hiphop-test
   (load-hiphop-test (hiphop-test-name->path "abcro"))))
