#lang racket

;;exercise 1.1
10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)

(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
  b
  a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
  ((< a b) b)
  (else -1))
  (+ a 1))

;;exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))(* 3 (- 6 2)(- 2 7)))

;;exercise 1.3
(define (sum-of-squares a b)(+ (* a a)(* b b)))
(define (do-two-largest f a b c)
  (cond ((and (<= a b)(<= a c) ) (f b c))
        ((and (<= b a)(<= b c) ) (f a c))
        (else (f a b))
  ) 
)
(define (sum-of-squares-of-two-largest a b c) (do-two-largest sum-of-squares a b c))

;;exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
#| 
  Instead of taking the absolute value of b,
  this code applies the + operation if b is positive, and applies the - operation if b is zero or negative. The point here is that we're conditionally picking which operation is applied based on the conditional.
|#

;;exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;: (test 0 (p))
#|
  Calling (p) hangs, since it just calls itself forever. Under applicative evaluation order, this code will attempt to evaluate (p) and hang. Under normal evaluation order, the short-circuit nature of if, means that y is never needed with these params, so (p) is never needed, never evaluated, and nver hangs (well, until someone calls test with non-zero x)
|#

;;exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;: (new-if (= 2 3) 0 5)

;: (new-if (= 1 1) 0 5)

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
#|
  The problem here is the else clause. Unlike with actual if, the else clause is evaluated here, so we end up infinitely recursing. You might actually be able to avoid something like this by rather than taking an else clause, take a procedure and evaluate it in that case.
|#

;;exercise 1.7

;;Original code
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
;; was missing this:
(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

#|
A small number example:
> (square 0.01)
0.0001
> (sqrt 0.0001)
0.03230844833048122

We consider "good enough" to be a an absolute error of 0.001. In this case, the square of the final result is  0.0010438358335233748, which is indeed within 0.001 of 0.001, even though it's clearly wrong. The issue here is that this absolute error value is massive compared to the numbers we're dealing with.

> (sqrt (square 1e154))
1e+154

This actually seems like it's working ok. If I go any higher, I bump up against the floating point double max, and get a hang, which isn't exactly a surprise, dealing with inf.
|#

;; a sqrt-iter that uses delta between guesses
(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
;; was missing this:
(define (square x) (* x x))

(define (good-enough? guess prev-guess )
  (define delta (abs (- guess prev-guess)))
  (< (/ delta guess) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 +inf.f x))

#|
> (sqrt (square 0.000001))
1.0000001034612418e-06

This gets me good results on small numbers.

> (sqrt (square 1e154))
1.0000000009645634e+154
On larger numbers, this results in less accurate results, because the relative delta is less than the previous absolute difference criteria we used.
|#

;;exercise 1.8
;; adapting the delta sqrt to cubert. Despite an itch to do so, we're not refactoring this to be more generic or neater.
(define (cube x) (* x x x))

(define (cubert-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (cubert-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (/ 
    (+ 
      (/ 
        x 
        (* guess guess)
      )
      ( * 2 guess)
    ) 
    3
  )
)

(define (good-enough? guess prev-guess )
  (define delta (abs (- guess prev-guess)))
  (< (/ delta guess) 0.001))

(define (cubert x)
  (cubert-iter 1.0 +inf.f x))
