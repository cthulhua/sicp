#lang racket
;;;SECTION 1.2.6

;; prime?

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (expt test-divisor 2) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;exercise 1.21
(smallest-divisor 199)
;; 199
(smallest-divisor 1999)
;; 1999
(smallest-divisor 19999)
;; 7

;; fast-prime?

(define (square n) (expt n 2))  
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;;EXERCISE 1.22
(define runtime current-inexact-milliseconds)

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (report-composite (- (runtime) start-time))
      ))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)

(define (report-composite elapsed-time)
  (display " !!! ")
  (display elapsed-time)
  false)

(define (timed-prime-test n)
  (newline)
  (display n)
  (display ": ")
  (start-prime-test n (runtime)))

(define (search-for-primes n c) 
  (if (> c 0)
    (if (timed-prime-test n)
      (search-for-primes (+ n 1) (- c 1))
      (search-for-primes (+ n 1) c))
    false)
)

(search-for-primes 100 3)
#|
> (search-for-primes 100 3)

100:  !!! 0.04296875
101:  *** 0.0009765625
102:  !!! 0.0009765625
103:  *** 0.0009765625
104:  !!! 0.0
105:  !!! 0.0
106:  !!! 0.0
107:  *** 0.0009765625#f
|#

(search-for-primes 1000 3)

#|
> (search-for-primes 1000 3)
1000:  !!! 0.003173828125
1001:  !!! 0.0
1002:  !!! 0.0
1003:  !!! 0.0
1004:  !!! 0.0
1005:  !!! 0.0
1006:  !!! 0.0
1007:  !!! 0.0009765625
1008:  !!! 0.0
1009:  *** 0.0009765625
1010:  !!! 0.0
1011:  !!! 0.0009765625
1012:  !!! 0.0009765625
1013:  *** 0.001953125
1014:  !!! 0.0
1015:  !!! 0.0009765625
1016:  !!! 0.0009765625
1017:  !!! 0.0
1018:  !!! 0.0
1019:  *** 0.0009765625#f
|#

(search-for-primes 10000 3)

#|
> (search-for-primes 10000 3)

10000:  !!! 0.0029296875
10001:  !!! 0.0029296875
10002:  !!! 0.0009765625
10003:  !!! 0.0
10004:  !!! 0.0
10005:  !!! 0.0009765625
10006:  !!! 0.0
10007:  *** 0.004150390625
10008:  !!! 0.0
10009:  *** 0.00390625
10010:  !!! 0.0
10011:  !!! 0.0
10012:  !!! 0.0
10013:  !!! 0.0009765625
10014:  !!! 0.0
10015:  !!! 0.0
10016:  !!! 0.0009765625
10017:  !!! 0.0009765625
10018:  !!! 0.0
10019:  !!! 0.001953125
10020:  !!! 0.0
10021:  !!! 0.0009765625
10022:  !!! 0.0
10023:  !!! 0.0
10024:  !!! 0.0
10025:  !!! 0.0
10026:  !!! 0.0
10027:  !!! 0.001953125
10028:  !!! 0.0
10029:  !!! 0.0
10030:  !!! 0.0
10031:  !!! 0.0009765625
10032:  !!! 0.0
10033:  !!! 0.0029296875
10034:  !!! 0.0
10035:  !!! 0.0
10036:  !!! 0.0
10037:  *** 0.0029296875#f
|#

;; hmmm 4 is close to root 10 (3.1622...)
;; > (/ 0.00390625 0.0009765625)
;; 4.0

;; when i do this with 1 mil and 10 mil it looks closer
;; > (/ 0.129150390625 0.041015625)
;; 3.1488095238095237

;; with larger numbers (1 bil and 10 bil) this ratio seems stable. bit off though, probably other stuff coming into play

;;exercise 1.23
(define (next-smallest-divisor n)
  (next-find-divisor n 2))

(define (next-find-divisor n test-divisor)
  (cond ((> (expt test-divisor 2) n) n)
        ((next-divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor )))))

(define (next-divides? a b)
  (= (remainder b a) 0))

(define (next-prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (cond ((= 2 n) 3)
        (else (+ n 2))))

(define (next-start-prime-test n start-time)
  (if (next-prime? n)
      (report-prime (- (runtime) start-time))
      (report-composite (- (runtime) start-time))
      ))
;; comparable at smaller numbers. timing larger
#|
> (for-each next-timed-prime-test '(101 103 107 1009 1013 1019 10007 10009 10037 100003 100019 100043))

101:  *** 0.036865234375
103:  *** 0.0
107:  *** 0.0009765625
1009:  *** 0.002197265625
1013:  *** 0.001953125
1019:  *** 0.001953125
10007:  *** 0.005126953125
10009:  *** 0.005126953125
10037:  *** 0.0048828125
100003:  *** 0.014892578125
100019:  *** 0.014892578125
100043:  *** 0.014892578125
> (search-for-primes 1000000000 3)

1000000007:  *** 1.953125
1000000009:  *** 1.875
1000000021:  *** 1.6650390625#f

> (for-each next-timed-prime-test '(1000000007 1000000009 1000000021))

1000000007:  *** 1.60986328125
1000000009:  *** 1.51708984375
1000000021:  *** 1.39306640625

I assume that this isn't .5 the time because of the additional function call and if check
|#
;;exercise 1.24

(define (fast-start-prime-test n start-time times)
  (if (fast-prime? n times)
      (report-prime (- (runtime) start-time))
      (report-composite (- (runtime) start-time))
      ))


(define (fast-timed-prime-test n times)
  (newline)
  (display n)
  (display ": ")
  (fast-start-prime-test n (runtime) times))

#|
> (for-each (lambda (n) (fast-timed-prime-test n 3)) '(101 103 107 1009 1013 1019 10007 10009 10037 100003 100019 100043))

101:  *** 0.02001953125
103:  *** 0.002197265625
107:  *** 0.0029296875
1009:  *** 0.003173828125
1013:  *** 0.0029296875
1019:  *** 0.0029296875
10007:  *** 0.00390625
10009:  *** 0.004150390625
10037:  *** 0.005126953125
100003:  *** 0.005126953125
100019:  *** 0.005126953125
100043:  *** 0.005859375
> (for-each (lambda (n) (fast-timed-prime-test n 3)) '(1000000007 1000000009 1000000021))

1000000007:  *** 0.01611328125
1000000009:  *** 0.011962890625
1000000021:  *** 0.013916015625

a lot faster with the larger numbers but slower with the smaller. i assume this has to do with the value of times
> (for-each (lambda (n) (fast-timed-prime-test n 10)) '(1000000007 1000000009 1000000021))

1000000007:  *** 0.04296875
1000000009:  *** 0.037841796875
1000000021:  *** 0.037841796875
seems to confirm that it scales with # of times you test

> (for-each (lambda (n) (fast-timed-prime-test n 1000)) '(1000000007 100043))

1000000007:  *** 3.7939453125
100043:  *** 1.823974609375
this does look like logarithmic growth tho!
|#
;;exercise 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;; this would work, but the numbers that we are exponentiating get huge fast and we probably end up doing arbitrary sized numerics math which tends to be slow

;;exercise 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
;; this is tree recursive instead of linear. ie we end up calling expmod way more
;; exercise 1.27

(define (square n) (expt n 2))  
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (exhaustive-fast-prime? n)
  (define (inner n a) 
    (cond ((= a 0) true)
          ((= (expmod a n n) a) (inner n (- a 1)))
          (else false)))
  (inner n (- n 1)))

(map exhaustive-fast-prime? '(561 1105 1729 2465 2821 6601))
#|
> (map exhaustive-fast-prime? '(561 1105 1729 2465 2821 6601))
'(#t #t #t #t #t #t)
cool! we are fooled
#|

