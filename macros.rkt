#lang racket

; Typed Racket doesn't help with macros

(provide listfn->variadic)

(define-syntax listfn->variadic
  (syntax-rules ()
    [(listfn->variadic f x ...) (f (list x ...))]))