#lang racket
;;;SECTION 1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;; Using sum

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

;: (sum-cubes 1 10)


(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

;: (sum-integers 1 10)


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;: (* 8 (pi-sum 1 1000))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;: (integral cube 0 1 0.01)

;: (integral cube 0 1 0.001)

;;exercise 1.29

(define (sum-simpsons term s_f s_a s_h a next b)
  (if (> a b)
      0
      (+ (term s_f s_a s_h b a)
         (sum-simpsons term s_f s_a s_h (next a) next b))))

(define (simpsons-term f a h n k)
  (define y (f (+ a (* k h))))
  (cond ((= 0 k) y)
        ((= n k) y)
        ((even? k) (* 2 y))
        (else (* 4 y)))        
  )

(define (simpsons f a b n)
  (define (inc n) (+ n 1))
  (define h (/ (- b a) n))
  ( * (/ h 3) (sum-simpsons simpsons-term f a h 0 inc n))
)

(define (cube x) (* x x x))
(simpsons cube 0 1 100)

;;exercise 1.30
(define (inc n) (+ n 1))
(define (sum term a next b)
  (define (iter a result)
    (if (= a b) 
        (+ result (term a))
        (iter (inc a) (+ result (term a)))))
  (iter a 0))
(define (cube x) (* x x x))
(sum cube 1 inc 10)

;;exercise 1.31
;;recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
(define (identity x) x)
;;5!
(define (inc n) (+ n 1))
(product identity 1 inc 5)
;;iterative
(define (inc n) (+ n 1))
(define (product term a next b)
  (define (iter a result)
    (if (= a b) 
        (* result (term a))
        (iter (next a) (* result (term a)))))
  (iter a 1))
;;5! again
(define (identity x) x)
(define (inc n) (+ n 1))
(product identity 1 inc 5)

(define (wallis-term n) 
  (if (= n 0)
    (/ 2 3)
    (/ (+ 2 (* 1 (if (odd? n) (+ n 1)  n )))
     (+ 3 (* 1 (if (odd? n) (- n 1) n))))))

(define (wallis n)
  (* 4.0 (product wallis-term 0 inc n)
  ))


;;exercise 1.32
;;iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (= a b) 
        (combiner result (term a))
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b)
)
(define (inc n) (+ n 1))
(sum identity 1 inc 10)

(define (product term a next b)
  (accumulate * 1 term a next b)
)
(product identity 1 inc 5)
;; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b)
)
(define (inc n) (+ n 1))
(sum identity 1 inc 10)

(define (product term a next b)
  (accumulate * 1 term a next b)
)
(product identity 1 inc 5)

;;exercise 1.33
;;iterative
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (define maybe-term (term a))
    (define new-term (if (filter maybe-term) maybe-term null-value))
    (if (= a b) 
        (combiner result new-term)
        (iter (next a) (combiner result new-term))))
  (iter a null-value))
(define (even-sum term a next b)
  (filtered-accumulate + even? 0 term a next b)
)
(define (inc n) (+ n 1))
(even-sum identity 1 inc 10)

(define (even-product term a next b)
  (filtered-accumulate * even? 1 term a next b)
)
(even-product identity 1 inc 5)

;; recursive
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (val_or_null predicate value null-value)
    (if (predicate value) value null-value))
  (if (> a b)
      null-value
      (combiner (val_or_null filter (term a) null-value)
         (filtered-accumulate combiner filter null-value term (next a) next b))))
