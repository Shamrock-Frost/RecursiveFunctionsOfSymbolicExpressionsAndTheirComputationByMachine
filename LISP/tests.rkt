#lang typed/racket

(require "original.rkt" "macros.rkt")

(define test1
  (parse-S-expression (open-input-string "(((F·X)·Y)·(G·Y))")))

(define test2
  (parse-S-expression (open-input-string "(A,B,C)")))

(define test3
  (parse-S-expression (open-input-string "((AB,C),D)")))

(define test4
  (parse-S-expression (open-input-string "((A,B),C,D·E)")))

(define test5
  (parse-M-expression (open-input-string "car[x]")))

(define test6
  (parse-M-expression (open-input-string "car[cons[(A·B);x]]")))

(define test7
  (parse-LISP-program (open-input-string "car[cons[(A·B);x]]")))

(define test8
  (parse-LISP-program
   (open-input-string
    (listfn->variadic string-join
                      "ff[x] = [atom[x]→x;T→ff[car[x]]]"
                      (string-append "subst[x;y;z] = [atom[z]→[eq[z;y]→x;T→z];"
                                                           "T→cons[subst[x;y;car[z]];"
                                                                  "subst[x;y;cdr[z]]]]")
                      "ff[subst[(X,Y,Z);A;(A·(B·C))]]"))))