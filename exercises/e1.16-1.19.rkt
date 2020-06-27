#lang racket
;;;SECTION 1.2.4

;; Linear recursion
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


;; Linear iteration
(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                (- counter 1)
                (* b product)))) 

;; Logarithmic iteration
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n
  (= (remainder n 2) 0)
)

;;exercise 1.16
(define (even? n
  (= (remainder n 2) 0))
)

(define (square n)
  (* n n)
)

(define (fast-expt-iter b n)
  (fei-inner 1 b n)
)

(define (fei-inner a b n)
  (cond ((= n 0) a)
        ((even? n) (fei-inner a (square b) (/ n 2)))
        (else (fei-inner (* a b) b (- n 1)))
  )
)

;;exercise 1.17
;;whoops skipped this one and did it iteratively

;;exercise 1.18
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (halve n) (/ n 2))
(define (double n) (* n 2))

(define (even? n
  (= (remainder n 2) 0))
)

(define (fast-mult-iter b n)
  (fmi-inner 0 b n)
)

(define (fmi-inner a b n)
  (cond ((= n 0) a)
        ((even? n) (fmi-inner a (double b) (halve n)))
        (else (fmi-inner (+ a b) b (- n 1)))
  )
)
;;exercise 1.19
;;wtf
; a <- bq + aq + ap
; b <- bp + aq
; twice:
; a <- (bpq + aqq) + (bqq + aqq + apq) + (bpq + apq + app)
; b <- (bpp + apq) + (bqq + aqq + apq)
; collect like terms
; a <- 2bpq + 2aq^2 + 2apq + bq^2 + ap^2
; b <- 2apq + bp^2 + bq^2 + aq^2 
; factor?
; a <- b(2pq + q^2) a(q^2 + 2pq) + a(p^2 + q^2)
; b <- b(p^2 + q^2) + a(q^2 + 2pq) 
; thus
; p' = (p^2 + q^2)
; q' = (q^2 + 2pq)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                  (+ (* p p) (* q q)); p' = (p^2 + q^2)
                  (+ (* q q) (* 2 p q)); q' = (q^2 + 2pq)
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
