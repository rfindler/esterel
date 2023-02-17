#lang racket

(provide load-hiphop-test
         parse-hiphop-output/string)

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/set
         html-parsing
         html)
(module+ test
  (require rackunit))


(define-logger hiphop)

; A Hiphop-module& is `(module& ,name ,input-signals ,output-signals ,@body)
; where
;   name           : Symbol
;   input-signals  : [List-of Symbol]
;   output-signals : [List-of Symbol]
;   body           : [List-of Esterel-expr&]


; parse-hiphop/string : String -> [List-of Hiphop-module&]
(define (parse-hiphop/string program)
  (parse-hiphop (open-input-string program)))
     
; parse-hiphop : Input-port -> [List-of Hiphop-module&]
(define (parse-hiphop port)
  (hiphop-parser (port->string port)))

(define (hiphop-parser str)
  (define prog (html->xexp str))
  (log-hiphop-debug "parsed ~a into ~a" str (pretty-format prog))
  (cons (aquire-top-module-name prog)
        (parse-modules prog)))

(define (aquire-top-module-name prog)
  (for*/first ([l (in-list prog)]
               [x (in-value (attempt-parse-top-module-name l))]
               #:when x)
    x))

(define (attempt-parse-top-module-name thing)
  (define l
    (and (string? thing)
         (regexp-match
          #px"exports\\.prg\\s+=\\s+new\\s+hh\\.ReactiveMachine\\((\\w+)\\s*,"
          thing)))
  (and l
       (string->symbol (second l))))
(module+ test
  (check-equal?
   (attempt-parse-top-module-name "exports.prg = new hh.ReactiveMachine(example, \"presentemit\");")
   'example))

(define (parse-modules prog)
  (let loop ([prog (remove-whitespace prog)])
    (match prog
      [(list* (? string? str)
              (and mod `(hh.module ,_ ...))
              rst)
       (cons (parse-module (parse-module-name str)
                           mod)
             (loop rst))]
      [(cons a b) (loop b)]
      [(? empty?) empty])))

(define (parse-module-name str)
  (string->symbol
   (third
    (regexp-match
     #px"(var\\s*|const\\s*)(\\w*)(\\s*=.*)"
     str))))

(module+ test
  (check-equal?
   (parse-module-name "var prg =\n")
   'prg)
  (check-equal?
   (parse-module-name "const prg =\n")
   'prg))
      
(define (remove-whitespace prog)
  (for/list ([x (in-list prog)]
             #:when (implies (string? x)
                             (not
                              (regexp-match?
                               #px"^\\s*$"
                               x))))
    x))
(module+ test
  (check-equal?
   (remove-whitespace
    `("var prg =\n"
      "    "
      (hh.module
       (@
        (a "${inSig}")
        (b "${inSig}")
        (c "${inSig}")
        (r "${inSig}")
        (o "${outSig}")))))
   `("var prg =\n"
     (hh.module
      (@
       (a "${inSig}")
       (b "${inSig}")
       (c "${inSig}")
       (r "${inSig}")
       (o "${outSig}"))))))

(define (parse-module modname mod)
  (match mod
    [`(hh.module (@ ,name-pairs ...)
                 ,body ...)
     (define-values (inputs outputs both)
       (parse-module-interface name-pairs))
     `(module& ,modname ,(reverse inputs) ,(reverse outputs) ,(reverse both)
               ,@(parse-hh-exprs body))]
    [`(hh.module  ,body ...)
     `(module& ,modname () () () ,@(parse-hh-exprs body))]))

(define (parse-module-interface name-pairs)
  (let loop ([name-pairs name-pairs]
             [inputs empty] [outputs empty] [both empty])
    (match name-pairs
      ['() (values inputs outputs both)]
      [(or (cons (list name (or "${sigIn}" "${inSig}"
                                "${{accessibility:hh.in}}"
                                "${{accessibility:hh.IN}}"))
                 rst)
           (list* (list name "${{accessibility:")
                  (or `(|hh.in}}|) `(|hh.IN}}|))
                  rst))
       (loop rst (cons name inputs) outputs both)]
      [(or (cons (list name (or "${outSig}" "${sigOut}"
                                "${{accessibility:hh.out}}"
                                "${{accessibility:hh.OUT}}"))
                 rst)
           (list* (list name "${{accessibility:")
                  (or `(|hh.out}}|) `(|hh.OUT}}|))
                  rst))
       (loop rst inputs (cons name outputs) both)]
      [(cons (list name) rst)
       (loop rst inputs outputs (cons name both))]
      [else (error 'parse-hiphop "cannot handle signal ~a" name-pairs)])))

(define (parse-hh-exprs body)
  (for/list ([b (in-list body)]
             #:unless (match b
                        [(? string?) #t]
                        [`(*COMMENT* ,_ ...) #t]
                        [_ #f]))
    (parse-hh-expr b)))

(define (parse-hh-expr b)
  (match b
    [`(hh.loopeach (@ (,sig)) ,@body)
     `(loop-each& ,sig ,@(parse-hh-exprs body))]
    [`(hh.parallel ,@body)
     `(par& ,@(parse-hh-exprs body))]
    [`(hh.await (@ (,sig)))
     `(await& ,sig)]
    [`(hh.await (@ . ,(list-no-order `(,sig) `(countvalue ,n))))
     `(await& ,(parse-number n) ,sig)]
    
    [`(hh.await (@ . ,(list-no-order `(,sig) `(immediate))))
     `(await-immediate& ,sig)]
    [`(hh.every (@ . ,(list-no-order `(,sig) `(countvalue ,n)))
                ,body ...)
     `(every& (,(parse-number n) ,sig)
              ,@(parse-hh-exprs body))]
    [`(hh.every (@ . ,(list-no-order `(,sig) `(immediate)))
                ,body ...)
     `(every& ,sig #:immediate
              ,@(parse-hh-exprs body))]
    
    [`(hh.emit (@ (,sig)))
     `(emit& ,sig)]
    [`(hh.local (@ (,sig)) ,body ...)
     `(signal& ,sig ,@(parse-hh-exprs body))]
    [`(hh.abort (@ (,sig)) ,body ...)
     `(abort& ,sig ,@(parse-hh-exprs body))]
    [`(hh.abort (@ (pre) (,sig)) ,body ...)
     `(abort& pre& ,sig ,@(parse-hh-exprs body))]
    [`(hh.sequence ,@body)
     `(seq& ,@(parse-hh-exprs body))]
    [`(hh.loop ,body ...)
     `(loop& ,@(parse-hh-exprs body))]
    [`(hh.nothing)
     `nothing&]
    [`(hh.pause)
     `pause&]
    [`(hh.halt)
     `halt&]
    [`(hh.if (@ (,sig)) ,@body)
     (match (parse-hh-exprs body)
       [(list a) `(present& ,sig ,a nothing&)]
       [(list a b) `(present& ,sig ,a ,b)])]
    [`(hh.if (@ (pre) (,sig)) ,@body)
     (match (parse-hh-exprs body)
       [(list a) `(present& pre& ,sig ,a nothing&)]
       [(list a b) `(present& pre& ,sig ,a ,b)])]
    [`(hh.run (@ (module ,mod)
                 ,name-pairs ...)
              ;; for some reason html-parsing parses
              ;; hh.run weirdly (probably cause its not actually html)
              ;; so bits of the remaining program might get spliced into the run
              ;; (probably because of how /> is parsed for non-html tags)
              ,again ...)
     `(seq&
       (run& ,(parse-mod-name-ref mod)
             ,@(parse-run-binding-pairs name-pairs))
       ,@(parse-hh-exprs again))]
    [`(hh.trap (@ (,T)) ,body ...)
     `(trap& ,T ,@(parse-hh-exprs body))]
    [`(hh.exit (@ (,T)))
     `(exit& ,T)]
    [`(hh.every (@ (,S)) ,body ...)
     `(every& ,S ,@(parse-hh-exprs body))]
    [`(hh.suspend (@ (,S)) ,body ...)
     `(suspend&  ,S ,@(parse-hh-exprs body))]
    [`(hh.sustain (@ (,S)))
     `(sustain& ,S)]
    [`(hh.local (@ (,S) ...) ,body ...)
     `(signal& (,@S) ,@(parse-hh-exprs body))]
    [`(hh.weakabort (@ (,S)) ,body ...)
     `(weak-abort& ,S ,@(parse-hh-exprs body))]
    [`(hh.weakabort (@ . ,(list-no-order `(,S) `(immediate))) ,body ...)
     `(weak-abort-immediate& ,S ,@(parse-hh-exprs body))]))

(define (parse-number s)
  (match s
    [(app string->number (? number? s)) s]
    [(app (curry regexp-match #rx"\\$\\{(.*)\\}")
          (list _ n))
     (parse-number n)]))
(module+ test
  (check-equal?
   (parse-number "${4}")
   4))
 

(define (parse-mod-name-ref m)
  (match m
    [(app (curry regexp-match #rx"\\$\\{(.*)\\}")
          (list _ n))
     (string->symbol n)]))
(module+ test
  (check-equal?
   (parse-mod-name-ref "${m1}")
   'm1))

(define (parse-run-binding-pairs p)
  (for/list ([p (in-list p)])
    (match p
      [(list s) (list s '-> s)]
      [(list s n)
       (list (parse-run-signal-name n) '-> s)])))

(define (parse-run-signal-name n)
  (string->symbol
   (string-downcase
    (string-trim n "/"))))
(module+ test
  (check-equal?
   (parse-run-signal-name "B/")
   'b))

(module+ test
  (check-equal?
   (parse-hiphop/string
    #<<DOC
"use hopscript"

var hh = require("hiphop");

var inSig = {direction: hh.IN};
var outSig = {direction: hh.OUT};

var prg =
    <hh.module A=${inSig} B=${inSig} C=${inSig} R=${inSig} O=${outSig}>
      <hh.loopeach R>
	<hh.parallel>
	  <hh.await A/>
	  <hh.await B/>
	  <hh.await C/>
	</hh.parallel>
	<hh.emit O/>
      </hh.loopeach>
    </hh.module>;

exports.prg = new hh.ReactiveMachine(prg, "ABCRO");

DOC
    )
   `(prg
     (module& prg (a b c r) (o) ()
              (loop-each& r
                          (par&
                           (await& a)
                           (await& b)
                           (await& c))
                          (emit& o)))))
  (check-equal?
   (parse-hiphop/string
    #<<DOC
"use hopscript"

var hh = require("hiphop");

var inSig = {accessibility: hh.IN};
var outSig = {accessibility: hh.OUT};

var prg = <hh.module I=${inSig} O=${outSig}>
  <hh.local L>
    <hh.parallel>
      <hh.abort L>
	<hh.loop>
	  <hh.emit O/>
	  <hh.pause/>
	</hh.loop>
      </hh.abort>
      <hh.sequence>
	<hh.await I/>
	<hh.emit L/>
      </hh.sequence>
    </hh.parallel>
  </hh.local>
</hh.module>;

exports.prg = new hh.ReactiveMachine(prg, "abortpar");

DOC
    )
   `(prg
     (module& prg (i) (o) ()
             (signal& l
               (par&
                (abort& l
                        (loop& (emit& o) pause&))
                (seq& (await& i) (emit& l)))))))
  (check-equal?
   (parse-hiphop/string
    #<<DOC
"use hopscript"

var hh = require("hiphop");

var m1 = <hh.module S U W Z>
  <hh.parallel>
    <hh.if S>
      <hh.emit W/>
    </hh.if>
    <hh.if U>
      <hh.emit Z/>
    </hh.if>
  </hh.parallel>
</hh.module>;

var inSig={accessibility: hh.IN}
var run2 = <hh.module S=${inSig} U=${inSig} A B>
  <hh.run module=${m1} S U W=A Z=B/>
</hh.module>;

exports.prg = new hh.ReactiveMachine(run2, "run2");

DOC
    )
   `(run2
     (module& m1 () () (s u w z)
              (par&
               (present& s (emit& w) nothing&)
               (present& u (emit& z) nothing&)))
     (module& run2
              (s u) () (a b)
              (seq& (run& m1 [s -> s] [u -> u] [a -> w] [b -> z])))))
  (check-equal?
   (parse-hiphop/string
    #<<DOC
"use hopscript"

var hh = require("hiphop");

var prg = <hh.module S1_and_S2 S1_and_not_S2 not_S1_and_S2 not_S1_and_not_S2>
  <hh.loop>
    <hh.trap T1>
      <hh.local S1>
	<hh.parallel>
	  <hh.sequence>
	    <hh.pause/>
	    <hh.emit S1/>
	    <hh.exit T1/>
	  </hh.sequence>
	  <hh.loop>
	    <hh.trap T2>
	      <hh.local S2>
		<hh.parallel>
		  <hh.sequence>
		    <hh.pause/>
		    <hh.emit S2/>
		    <hh.exit T2/>
		  </hh.sequence>
		  <hh.loop>
		    <hh.sequence>
		      <hh.if S1>
			<hh.if S2>
			  <hh.emit S1_and_S2/>
			  <hh.emit S1_and_not_S2/>
			</hh.if>
			<hh.if S2>
			  <hh.emit not_S1_and_S2/>
			  <hh.emit not_S1_and_not_S2/>
			</hh.if>
		      </hh.if>
		      <hh.pause/>
		    </hh.sequence>
		  </hh.loop>
		</hh.parallel>
	      </hh.local>
	    </hh.trap>
	  </hh.loop>
	</hh.parallel>
      </hh.local>
    </hh.trap>
  </hh.loop>
</hh.module>;

exports.prg = new hh.ReactiveMachine(prg, "P18");
DOC
    )
   `(prg
     (module& prg () () (s1_and_s2 s1_and_not_s2 not_s1_and_s2 not_s1_and_not_s2)
              (loop&
               (trap& t1
                 (signal& s1
                   (par&
                    (seq& pause& (emit& s1) (exit& t1))
                    (loop&
                     (trap& t2
                       (signal& s2
                         (par&
                          (seq& pause& (emit& s2) (exit& t2))
                          (loop&
                           (seq&
                            (present& s1
                                      (present& s2
                                                (emit& s1_and_s2)
                                                (emit& s1_and_not_s2))
                                      (present& s2
                                                (emit& not_s1_and_s2)
                                                (emit& not_s1_and_not_s2)))
                            pause&))))))))))))))
              

; A Hiphop-output is [List-of Hiphop-output-item]

; A Hiphop-output-item is one of:
; - '!reset
; - `(,[List-of Symbol] => ,[List-of Symbol])

(define maybe-downcase (make-parameter values))

(define-tokens hiphop-output-tokens
  (PROMPT SIGNAL))

(define-empty-tokens hiphop-output-empty-tokens
  (OUTPUT SEMICOLON RESET-COMMAND RESET-RESPONSE OUTPUT-EOF
          BANGS
          BANGS3
          WARNING))

(define-lex-abbrev output-identifier (:+ (:or alphabetic numeric #\_)))

(define hiphop-output-lexer
  (lexer
   (#\; (token-SEMICOLON))
   ("!!!" (token-BANGS3))
   ((:: "!!!" (:+ "!")) (token-BANGS))
   ("--- Output:" (token-OUTPUT))
   ("Warning: Potential causality cycle" (token-WARNING))
   ("!reset" (token-RESET-COMMAND))
   ((:: output-identifier #\>)
    (token-PROMPT lexeme))
   (output-identifier
    (token-SIGNAL (string->symbol lexeme)))
   ((:: "--- Automaton " output-identifier " reset")
    (token-RESET-RESPONSE))
   (whitespace (hiphop-output-lexer input-port))
   ((eof) (token-OUTPUT-EOF))))

(define hiphop-output-parser
  (parser
   (start sequence)
   (end OUTPUT-EOF BANGS3)
   (error void)
   (tokens hiphop-output-tokens hiphop-output-empty-tokens)
   (grammar
    (sequence
      [() '()]
      [(causality) (list $1)]
      [(WARNING sequence) $2]
      [(cycle sequence) (cons $1 $2)])
    (causality
     [(PROMPT signals SEMICOLON BANGS)
      `(,$2 => #:causality-error)])
    (cycle
     [(PROMPT signals SEMICOLON OUTPUT signals)
      `(,$2 => ,$5)]
     [(PROMPT RESET-COMMAND SEMICOLON RESET-RESPONSE)
      '!reset])
    (signals
     [() '()]
     [(SIGNAL signals) (cons (string->symbol ((maybe-downcase) (symbol->string $1)))
                             $2)]))))

; parse-hiphop-output : Input-port -> Hiphop-output
(define (parse-hiphop-output port*
                             #:downcase? [downcase? #f])
  (define port port*)
  (log-hiphop-debug "parsing test inputs: ~a"
                    (let-values ([(s) (open-output-string)]
                                 [(i o) (make-pipe)])
                      (copy-port port s o)
                      (close-output-port o)
                      (set! port i)
                      (get-output-string s)))
  (parameterize ([maybe-downcase (if downcase? string-downcase values)])
    (hiphop-output-parser (λ () (hiphop-output-lexer port)))))

; parse-hiphop-output/string : String -> Hiphop-output
(define (parse-hiphop-output/string output #:downcase? [downcase? #f])
  (parse-hiphop-output (open-input-string output)
                       #:downcase? downcase?))

(module+ test
  (define o
    (lambda (x)
      (parse-hiphop-output/string x #:downcase? #t)))

  (check-equal? (o "abortpar> ;
                    --- Output: O\n")
                '((() => (o))))
  (check-equal? (o "abortpar> ;
                    --- Output: O
                    abortpar> I;
                    --- Output:
                    abortpar> ;
                    --- Output: \n")
                '((() => (o)) ((i) => ()) (() => ())))
  (check-equal? (o "ABCRO> C B;
                    --- Output: O R S\n")
                '(((c b) => (o r s))))
  (check-equal? (o "abortpar> ;
                    --- Output:
                    abortpar> !reset;
                    --- Automaton abortpar reset
                    abortpar> ;
                    --- Output: O\n")
                '((() => ()) !reset (() => (o)))))

; hiphop-output->input-signals : Hiphop-output -> [List-of Symbol]
; Gets all the symbols used as input signals in a hiphop.js output log.
(define (hiphop-output->input-signals output)
  (apply set-union (map first (filter cons? output))))

; hiphop-output->output-signals : Hiphop-output -> [List-of Symbol]
; Gets all the symbols used as output signals in a hiphop.js output log.
(define (hiphop-output->output-signals output)
  (apply set-union (map third (filter cons? output))))

; load-hiphop-modules : path-string? -> [List-of Hiphop-module&]
(define (load-hiphop-modules path)
  (call-with-input-file* path parse-hiphop))

; load-hiphop-output : path-string? -> Hiphop-output
(define (load-hiphop-output path #:downcase? [downcase? #f])
  (call-with-input-file (path-replace-extension path ".out")
    (curry parse-hiphop-output #:downcase? downcase?)))

; split-hiphop-output : Hiphop-output -> [List-of Hiphop-output]
; Splits hiphop output on resets.
(define (split-hiphop-output output)
  (define-values (x xs) (splitf-at output cons?))
  (if (empty? xs)
      (list x)
      (cons x (split-hiphop-output (rest xs)))))

; hiphop-module->machine : Hiphop-module& -> S-expression
(define (hiphop-module->machine module)
  (match-define `(module& ,name
                          ,input-signals ,output-signals ,input/outputs
                          ,@body)
    module)
  `(define-esterel-machine ,name
     #:inputs ,input-signals
     #:outputs ,output-signals
     #:input/outputs ,input/outputs
     (seq& ,@body)))

; hiphop-output->tests : Symbol Hiphop-output -> S-expression
(define (hiphop-output->tests name output)
  (map (λ (script) `(test-seq ,name ,@script))
       (split-hiphop-output output)))

; load-hiphop-tests : path-string? -> S-expression
(define (load-hiphop-test path)
  (define modules         (load-hiphop-modules path))
  (define output          (load-hiphop-output path      
                                              #:downcase? #t))
  (define machine-name    (first modules))
  `(test-case ,(format "~a" path)
     ,@(map hiphop-module->machine (rest modules))
     ,@(hiphop-output->tests machine-name output)))
