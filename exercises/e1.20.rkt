#lang racket
;;;SECTION 1.2.5

;;exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
;; normal order:
;; (gcd 206 40)
;;  if...
;;  (remainder 206 40)
;;  if...
;;  (remainder 40 (remainder 206 40))
;;  if...
;;  (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;  if...
;;  (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;; remainder is called 18 times.
;; 14 executions evaluated as part of if's and 4 as part of the reduction 
;; i thought that this never terminated, but it's just because I misunderstood how to handle the 'if' special form.
;; it does terminate; just takes a while doing a ton of redundant remainder operations as part of handling the if's
;; applicative order:
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)
;; 2
;; remainder is called 4 times.
