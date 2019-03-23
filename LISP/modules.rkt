#lang typed/racket

(require "m-expr-lang.rkt" "../util.rkt")

(provide (all-defined-out))

; This file implements some parsing functions for convenience

; A "library" is a lisp program whose last expression is the atom EXPORT
(: load-library (-> LISP-program Env))
(define (load-library prog)
  (unless (equal? (LISP-program-last-expr prog) (atom "EXPORT"))
    (error 'load-library
           "Program ~a doesn't mark itself with EXPORT, cannot be loaded as library" prog))
  (add-fundefs-to-env empty-env (LISP-program-definitions prog)))

(: path->LISP-program (-> Path-String LISP-program))
(define path->LISP-program
  (compose parse-LISP-program open-input-file))

(: load-library-from-path (-> Path-String Env))
(define load-library-from-path
  (compose load-library path->LISP-program))

(: repl (->* () (Env) Void))
(define (repl [env empty-env])
  (define input (read-line))
  (drain-port (current-input-port))
  (cond
    [(or (equal? input "QUIT") (equal? input eof)) (void)]
    [(string-contains? input "=")
     (let ([prog (parse-LISP-program
                  (open-input-string
                   (string-append input " " "VOID")))])
       (repl (add-fundefs-to-env env (LISP-program-definitions prog))))]
    [else
     (displayln
      (pretty-print-S-expr
       (eval-expression-under-env
        env
        (LISP-program-last-expr
         (parse-LISP-program (open-input-string input))))))
     (repl env)]))