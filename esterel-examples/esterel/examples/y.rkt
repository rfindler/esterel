#lang racket/gui
(require esterel/full net/http-client json)

(define-signal
  text #:init "" #:combine string-append
  recv-en #:init "" #:combine string-append)

(define (exec-color lang-pair)
  (pause)
  (debug-when-must (printf "exec-color.pause\n"))
  (when (present? recv-en) (debug-when-must (printf "exec-color.3\n")))
  (debug-when-must (printf "exec-color.4\n"))
  (signal-value recv-en)
  )

(define r
  (esterel
   (begin
     (present? text)
     (suspend (exec-color "fr|en")
              (present? text)))))

(react! r #:emit (list (cons text "f")))
(react! r #:emit (list (cons recv-en "g")))
