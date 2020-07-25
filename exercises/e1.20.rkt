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
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; AFAICT this just keeps shuffling around forever; part of normal order evaluation is that you continue to "fully expand" until there's only primitive operators, and then you "reduce", but the second branch of gcd involves invoking gcd again on b and remainder, but if we always have gcd, and don't evaluate remainder until we only have primitive operators, then we never evaluate remainder, and thus also never get to take the other branch.
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
