#lang typed/racket

(provide option-map expect
         expect-one-of consume-if-peeked
         drain-port
         hash-union)

(: to-bool (All (A) (-> A Boolean)))
(define (to-bool x) (if x #t #f))

(: option-map (All (A B) (-> (-> A B) (U A False) (U B False))))
(define (option-map f x)
  (and x (f x)))

(: expect-one-of (-> (Listof Char) Input-Port Void))
(define (expect-one-of acceptable inp)
  (define actual (read-char inp))
  (unless (member actual acceptable)
    (error 'expect "Expected any of ~a but got ~a" acceptable actual)))

(: expect (-> Char Input-Port Void))
(define (expect expected inp)
  (expect-one-of (list expected) inp))

(: consume-if-peeked (-> (U Char EOF) Input-Port Boolean))
(define (consume-if-peeked c inp)
  (if (eof-object? c)
      (eof-object? (peek-char inp))
      (to-bool
       (and (equal? c (peek-char inp))
            (expect c inp)))))

(: drain-port (-> Input-Port Void))
(define buf-size 20000)
(define (drain-port port)
  (define buf (make-bytes buf-size))
  (let loop ()
    (define try-read (read-bytes-avail!* buf port))
    (cond [(or (eof-object? try-read)
               (and (number? try-read) (= try-read 0)))
           (void)]
          [else
           (loop)])))

(: hash-union (All (a b)
                   (-> (Immutable-HashTable a b)
                       (Immutable-HashTable a b)
                       (Immutable-HashTable a b))))
(define (hash-union h0 h)
  (for/fold ([res : (Immutable-HashTable a b) h0])
            ([(k v) h])
    (hash-set res k v)))