#lang typed/racket

#|

  This program is an implementation of LISP as described in the seminal paper,
  "Recursive Functions of Symbolic Expressions and Their Computation by Machine"
  It is implemented in Typed Racket, a Racket derivative.
  Racket being the successor to Scheme.
  Scheme being the MIT LISP derivative inspired by LISP 1.5 and Maclisp
  LISP 1.5 and Maclisp were both successors to LISP 1
  LISP 1 is not the language implemented here, as it forgoes M-expressions entirely,
but it was the first actual implementation derived from the aforementioned paper describing LISP.
  The Lisp family has many dialects, and I've chosen to implement the original LISP
in a weird, complicated, and even typed(!) dialect in order to show how much modern
Lisps have diverged from the language implemented herein.

|#

(require "../util.rkt")
(provide (all-defined-out))

#|

  The LISP language was initially developed by John McCarthy in the 1956-1958.
  This makes it the second "high-level" programming language, after FORTRAN.
  This makes it the first simple programming language.
  LISP arose out of an interest in AI programming, and out of a desire for
programming to be focused on symbols instead of just numbers.
  LISP was not created with the goal of creating the programming
language to end all programming languages, it had a much simpler
goal in mind.
  LISP was built as a way to assist in developing a system called
the Advice Taker, a primitive AI developed from 1958 at MIT.
  However, in the course of its development, LISP went beyond this.
  Unlike its predecessor FORTRAN, LISP was described with a short,
simple representation that was independent of a certain computer.
  This file implements the bare version of LISP described in "Recursive
Functions of Symbolic Expressions and Their Computation by Machine".

|#

#|

  In the original LISP paper, McCarthy defines an atom to be a string of
uppercase latin characters, digits, and spaces, with consecutive spaces
disallowed. He also remarks that atoms could be anything which has a notion
of equality, like numbers.
  Atoms are then used to describe S-expressions, which form the syntax for the LISP
language. S-Expressions are binary trees whose leaves are atoms, where the tree
with left subtree L and right R is written (L·R). We represent these strings with
the dot struct above. We build all of LISP out of ordered pairs and atoms.

|#

(: parse-atom (-> Input-Port (U String False)))
(define (parse-atom inp)
  (define matches (regexp-try-match #rx"^[A-Z0-9]( ?[A-Z0-9])*" inp))
  (and matches (bytes->string/latin-1 (car matches))))

(define-type S-expr (U dot atom))
(struct dot ([left : S-expr] [right : S-expr]) #:transparent)
(struct atom ([str : String]) #:transparent)

; We cheat a little by converting it to racket
(: pretty-print-S-expr (-> S-expr String))
(define (pretty-print-S-expr s)
  (: to-racket (-> S-expr Any))
  (define (to-racket s-exp)
    (if (dot? s-exp)
        (cons (to-racket (dot-left s-exp))
              (to-racket (dot-right s-exp)))
        (let ([sym (atom-str s-exp)])
          (if (equal? sym "NIL")
              null
              (string->symbol sym)))))
  (format "~a" (to-racket s)))

; S-expression Parser
(: parse-S-expression (-> Input-Port S-expr))
(define (parse-S-expression inp)
  (or (option-map atom (parse-atom inp))
      (begin
        (expect #\( inp)
        (let ([left (parse-S-expression inp)])
          (expect #\· inp)
          (let ([right (parse-S-expression inp)])
            (expect #\) inp)
            (dot left right))))))

; We extend the base language of S-Expressions with special list syntax
; A list in LISP is a sequence (x1·(x2·...(xn·NIL))) for expressions x1,x2,..xn
; NIL is just a distinguished atom 
; The syntax (x1,x2,..,xn) represents the list (x1·(x2·...(xn·NIL)))
; The syntax (x1,x2,..,xn·y) represents the expression (x1·(x2·...(xn·y)))
; As a special case where n=1, we recover the syntax (L·R)
(set! parse-S-expression
      (λ ([inp : Input-Port])
        (: loop (-> S-expr))
        (define (loop)
          (let ([next-expr (parse-S-expression inp)]
                [next-char (read-char inp)])
            (dot next-expr
                 (cond
                   [(equal? next-char #\)) (atom "NIL")]
                   [(equal? next-char #\·)
                    (let ([tail (parse-S-expression inp)])
                      (expect #\) inp)
                      tail)]
                   [(equal? next-char #\,) (loop)]
                   [else (error "Expected ) or , or ) in S-expression")]))))
        (or (option-map atom (parse-atom inp))
            (begin
              (expect #\( inp)
              (loop)))))

#|

  After defining S-expressions, McCarthy describes functions over S-expressions,
the syntax for which he calls M-expressions (the M meaning "meta").
  M-expressions, unlike anything in modern lisp, use the more traditional
function applications syntax but with bracket, e.g. `f[(A,B,C)]` denotes the
function `f` applied to the S-expression `(A,B,C)`, or `(A·(B·(C·NIL)))`.
  To distinguish them from atoms, all functions and S-expression variables are
written with lowercase variables. Semicolons are used in argument lists.

|#

(: parse-fun-or-var (-> Input-Port (U String False)))
(define (parse-fun-or-var inp)
  (define matches (regexp-try-match #rx"^[a-z0-9]+" inp))
  (and matches (bytes->string/latin-1 (car matches))))

(define-type M-expr (U var funcall S-expr))
(struct var ([name : String]) #:transparent)
(struct funcall ([function : var]
                 [args : (Listof M-expr)])
  #:transparent)
(: parse-M-expression (-> Input-Port M-expr))
(define (parse-M-expression inp)
  (: argloop (-> (Listof M-expr)))
  ; assumes there is no [ at the start and there is at least one argument
  (define (argloop)
    (let ([next-expr (parse-M-expression inp)]
          [next-char (read-char inp)])
      (cons next-expr
            (cond
              [(equal? next-char #\]) null]
              [(equal? next-char #\;) (argloop)]
              [else (error "Expected ] or ; in M-expression")]))))
  (let ([ident (parse-fun-or-var inp)])
    (if ident
        (let ([peek (peek-char inp)])
          (if (equal? #\[ peek)
              (begin
                (expect #\[ inp)
                (funcall (var ident)
                         (if (consume-if-peeked #\] inp)
                             null
                             (argloop))))
              (var ident)))
        (parse-S-expression inp))))

#|

  Our final piece of syntax is a method of defining (possibly recursive) functions of S-exprs
using conditionals. If e1,e2,...,en and c1,c2,..,cn are M-expressions with free variables
x1,x2,...,xn and each of the ck evaluates to either T or F whenever it is defined, then
f[x1,x2,..,xn] = [c1→e1;c2→e2;...;cn→en] defines the function f such that for all S-expressions
s1,s2,...sn, f[s1,s2,..,sn] is e1 with all x_k substituted for s_k if c1 is the atom T
when each x_k is substited for s_k, is e2 with all x_k substituted for s_k if the corresponding
condition holds for c2, and so on. If no conditions are met, the function is undefined on that
input. The c_k and e_k may refer to f in their bodies, and if an infinite recursion occurs,
f is considered undefined on that input.
  Fun fact: LISP was the first language to have the modern form of conditionals!
  FORTRAN didn't have booleans (though whether LISP did is debateable), but instead
a three way branching construct called the "arithmetic if" which chose a branch
based on whether its input was zero, positive, or negative.
  Our final AST for LISP is a series of function definitions (separated by spaces or newlines)
followed by an M-expression.

|#
(define-type conditional-expr (U conditional M-expr))
(struct conditional ([branches : (Listof (Pairof conditional-expr conditional-expr))])
  #:transparent)

(: parse-conditional-expr (-> Input-Port conditional-expr))
(define (parse-conditional-expr inp)
  (: parse-conditional-body (-> (Listof (Pairof conditional-expr conditional-expr))))
  (define (parse-conditional-body)
    (let ([cond-expr (parse-conditional-expr inp)])
      (expect #\→ inp)
      (let ([result-expr (parse-conditional-expr inp)]
            [next-char (read-char inp)])
        (cons (cons cond-expr result-expr)
              (cond
                [(equal? next-char #\]) null]
                [(equal? next-char #\;) (parse-conditional-body)]
                [else
                 (error "Expected ] or ; in conditional")])))))
  (if (consume-if-peeked #\[ inp)
      (conditional (parse-conditional-body))
      (parse-M-expression inp)))

(struct fundef ([name : var]
                [arg-variables : (Listof var)]
                [body : conditional-expr])
  #:transparent)
(struct LISP-program ([definitions : (Listof fundef)]
                      [last-expr : M-expr])
  #:transparent)
(: parse-LISP-program (-> Input-Port LISP-program))
(define (parse-LISP-program inp)
  (: parse-fundef (-> M-expr fundef))
  (define (parse-fundef head)
    (unless (and (funcall? head)
             (andmap var? (funcall-args head)))
      (error "Expected function name and names of args in fundef"))
    (expect #\space inp)
    (expect #\= inp)
    (expect #\space inp)
    (fundef (funcall-function head)
            (funcall-args head)
            (parse-conditional-expr inp)))
  (define first-expr (parse-M-expression inp))
  (if (consume-if-peeked eof inp)
      (LISP-program null first-expr)
      (let ([curr-def (parse-fundef first-expr)])
        (expect-one-of (list #\space #\newline) inp)
        (let ([rest-of-program (parse-LISP-program inp)])
          (LISP-program (cons curr-def (LISP-program-definitions rest-of-program))
                        (LISP-program-last-expr rest-of-program))))))
  
#|

  With our terms defined, we can start to talk about meaning.
  LISP has five primitive functions:
    - atom[x] returns the atom T if x is an atom, or F otherwise
    - eq[x;y] is defined iff x and y are atoms.
      In that case, it is T iff x and y are the same atom, and is F otherwise
    - car[x] is defined iff x is not an atom, and car[x·y] is x
    - cdr[x] is defined iff x is not an atom, and cdr[x·y] is y 
    - cons[x;y] is x·y
  car and cdr are very silly names and it's absurd how common they still are. 
  The terms are mnemonics for their original implementation
on the IBM 704:
    - car stands for "contents of the address part of register"
    - cdr stands for "contents of the decrement part of register"

|#

(define-type Env (Immutable-HashTable var (U fundef S-expr)))
(: empty-env Env)
(define empty-env ((inst hash var (U fundef S-expr))))

(: eval-expression-under-env (-> Env conditional-expr S-expr))
(define (eval-expression-under-env env expr)
  (match expr
    [(struct var (n))
     (if (hash-has-key? env (var n))
         (let ([lookup (hash-ref env (var n))])
           (if (or (atom? lookup) (dot? lookup)) ; hack to get occurence typing working
               lookup
               (error 'eval-M-expression-under-env
                      "Variable ~a used in expression context but bound to function ~a"
                      n lookup)))
         (error 'eval-M-expression-under-env
                "Couldn't find variable ~a in environment ~a"
                n env))]
    [(funcall (struct var ("atom")) (list x))
     (if (atom? (eval-expression-under-env env x)) (atom "T") (atom "F"))]
    [(funcall (struct var ("eq")) (list x y))
     (let ([x* (eval-expression-under-env env x)]
           [y* (eval-expression-under-env env y)])
         (if (and (atom? x*) (atom? y*))
             (if (equal? x* y*) (atom "T") (atom "F"))
             (error 'eval-M-expression-under-env
                    "eq called on non-atoms ~a and ~a in environment ~a"
                    x* y* env)))]
    [(funcall (struct var ((and c_r (or "car" "cdr")))) (list x))
     (let ([x* (eval-expression-under-env env x)])
       (if (dot? x*)
           (match c_r ["car" (dot-left x*)] ["cdr" (dot-right x*)])
           (error 'eval-M-expression-under-env
                  "~a called on atom ~a in environment"
                  c_r x*)))]
    [(funcall (struct var ("cons")) (list x y))
     (dot (eval-expression-under-env env x) (eval-expression-under-env env y))]
    [(funcall n arg-exprs)
     (if (hash-has-key? env n)
         (let ([lookup (hash-ref env n)])
           (if (fundef? lookup)
               (let* ([arg-names (fundef-arg-variables lookup)]
                      [expected-num-args (length arg-names)]
                      [actual-num-args (length arg-exprs)])
                 (unless (equal? expected-num-args actual-num-args)
                   (error 'eval-M-expression-under-env
                          "Function ~a has arity ~a but was given ~a arguments in environment ~a"
                          n expected-num-args actual-num-args env))
                 (let* ([body (fundef-body lookup)]
                        [arg-s-exprs (map (curry eval-expression-under-env env) arg-exprs)]
                        [new-env (foldr (λ ([arg-name : var] [arg-expr : S-expr] [curr-env : Env])
                                          (hash-set curr-env arg-name arg-expr))
                                        env arg-names arg-s-exprs)])
                   ; The following line isn't a bug
                   ; LISP as originally described had dynamic scope!
                   ; We evaluate the body under the current env, without a closure
                   ; At SPLASH '17, Gul Agha related that he had once asked McCarthy why
                   ; LISP had dynamic scope. McCarthy replied
                   ; "To be honest, I didn’t understand lambda calculus."
                   ; Source: http://archive.is/SJGCj
                   (eval-expression-under-env new-env body)))
               (error 'eval-M-expression-under-env
                      "Variable ~a used in function context but bound to expression ~a"
                      n lookup)))
         (error 'eval-M-expression-under-env
                "Couldn't find variable ~a in environment ~a"
                n env))]
    [(conditional (list-rest branches))
     (or
      (for/or : (U S-expr False) ([branch branches])
        (match (eval-expression-under-env env (car branch))
          [(atom "T") (eval-expression-under-env env (cdr branch))]
          [(atom "F") #f]
          [other (error 'eval-expression-under-env
                        "Branch condition ~a evaluated to non-boolean ~a in ~a under environment ~a"
                        (car branch) other expr env)]))
      (error 'eval-expression-under-env "No branches in ~a matched expr" expr))]
    ; Typed Racket doesn't have full support for match yet
    ; So even though the cases where expr is a conditional, funcall, or var have been covered,
    ; it can't infer that the union type conditional-expr = (U conditional funcall var S-expr)
    ; can be safely coerced to S-expr
    [s-expr (cast s-expr S-expr)]))

(: eval-expression (-> conditional-expr S-expr))
(define eval-expression (curry eval-expression-under-env empty-env))

(: add-fundefs-to-env (-> Env (Listof fundef) Env))
(define (add-fundefs-to-env env fundefs)
  (foldl (λ ([defn : fundef] [curr-env : Env])
           (hash-set curr-env (fundef-name defn) defn))
         env
         (reverse fundefs)))

(: eval-LISP-program-under-env (-> Env LISP-program S-expr))
(define (eval-LISP-program-under-env env lisp-program)
  (eval-expression-under-env
   (add-fundefs-to-env env (LISP-program-definitions lisp-program))
   (LISP-program-last-expr lisp-program)))

(: eval-LISP-program (-> LISP-program S-expr))
(define eval-LISP-program (curry eval-LISP-program-under-env empty-env))

#|

  Voila, a programming language ex-littleo in 331 lines (with annotations).
  LISP is turing complete and more than just a turing tarpit.
  You can write real programs in it! And maybe people did.
  Historically, LISP has been used extensively in AI research, in the pre-Machine Learning
era of Symbolic AI. The ELIZA "therapist" chatbot was written in lisp.

|#

; And that's an whole LISP!
