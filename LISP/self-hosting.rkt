#lang typed/racket

(require "m-expr-lang.rkt" "modules.rkt" "../util.rkt")

#|

  The beauty of LISP is not just that it builds
from atoms, dots, conditions, and 5 forms, however.
  The beauty of LISP is not just that it builds itself.
  This file hoists the object level up to the meta level,
or perhaps the other way around.

|#

(: list->S-expr (-> (Listof S-expr) S-expr))
(define (list->S-expr ls)
  (foldr dot (atom "NIL") ls))

(: meta->object (-> conditional-expr S-expr))
(define (meta->object expr)
  (cond
    [(or (atom? expr) (dot? expr))
     (dot (atom "QUOTE") (dot expr (atom "NIL")))]
    [(var? expr) (atom (string-upcase (var-name expr)))]
    [(funcall? expr)
     (list->S-expr
      (map meta->object
           (cons (funcall-function expr)
                 (funcall-args expr))))]
    [(conditional? expr)
     (list->S-expr
      (cons (atom "COND")
            (map (λ ([pair : (Pairof conditional-expr conditional-expr)])
                   (dot
                    (meta->object (car pair))
                    (dot (meta->object (cdr pair))
                         (atom "NIL"))))
                 (conditional-branches expr))))]))

(: fundef->object (-> fundef S-expr))
(define (fundef->object defn)
  (list->S-expr
   (list 
    (atom "LABEL")
    (meta->object (fundef-name defn))
    (list->S-expr
     (list (atom "LAMBDA")
           (list->S-expr
            (map meta->object (fundef-arg-variables defn)))
           (meta->object (fundef-body defn)))))))
(: LISP-program->objects (-> LISP-program S-expr))
(define (LISP-program->objects prog)
  (foldr dot
         (dot (meta->object
               (LISP-program-last-expr prog))
              (atom "NIL"))
         (map fundef->object (LISP-program-definitions prog))))

#|

  We are now ready for the core of lisp. In apply.lisp,1
we have defined and M-expression apply such that
for any M-function call f[args], where args is a list
f[args] = apply[f*;args], where f* is the meta->object
translation of f as above.
  Reading this equality backwards, apply is a LISP interpreter!
  As a demonstration, consider the following execution of ff[(A·B)],
where ff[x] = [atom[x]→x;T→ff[car[x]]]

|#
(define (example)
  (define prelude-env (load-library-from-path "prelude.lisp1"))
  (define apply-env (load-library-from-path "apply.lisp1"))
  (define expr
    (parse-conditional-expr
     (open-input-string "apply[(LABEL,FF,(LAMBDA,(X),(COND,((ATOM,X),X),((QUOTE,T),(FF,(CAR,X))))));((A·B))]")))
  (eval-expression-under-env
   (hash-union prelude-env apply-env)
   expr))