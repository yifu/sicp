#lang sicp
(#%require (only racket/base random))

;;Procedures and the Processes They Generate

;; Exercice 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n)) ;; f computes 2n

(define (g n) (A 1 n)) ;; g computes 2^n

(define (h n) (A 2 n)) ;; h computes 2^2^2...^2 (n times)

(define (k n) (* 5 n n))

;; Exercice 1.11

;;(define (f n)
;;  (cond ((< n 3) n)
;;        (else (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))))

(define (fib n)
  (f-iter 2 1 0 n))

(define (f-iter a b c n)
  (if (= n 0)
      c
      (f-iter (+ a b c) a b (- n 1))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)

;;0
;;1
;;2
;;3
;;6
;;11
;;20
;;37
;;68
;;125
;;230


;; Exercice 1.12

(define (pascal line pos)
  (cond ((= line 0) 1)
        ((= pos 0) 1)
        ((= pos line) 1)
        (else (+ (pascal (- line 1) (- pos 1)) (pascal (- line 1) pos)))))
      
      
;; Exercice 1.14
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
; (define (first-denomination kinds-of-coins)
 ; (cond ((= kinds-of-coins 1) 1)
  ;      ((= kinds-of-coins 2) 5)
   ;     ((= kinds-of-coins 3) 10)
    ;    ((= kinds-of-coins 4) 25)
     ;   ((= kinds-of-coins 5) 50)))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 50)
        ((= kinds-of-coins 2) 25)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 5)
        ((= kinds-of-coins 5) 1)))


;; Exercice 1.16
(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter acc b n)
  (cond ((= n 0) acc)
        ((even? n) (fast-expt-iter acc (square b) (/ n 2)))
        (else (fast-expt-iter (* b acc) b (- n 1)))))

(newline)
(fast-expt-iter 1 2 2) ;; 4
(fast-expt-iter 1 2 3) ;; 8
(fast-expt-iter 1 2 4) ;; 16
(fast-expt-iter 1 3 2) ;; 9
(fast-expt-iter 1 3 1) ;; 3

;; Exercice 1.17
(newline)
(define (halve x)
  (/ x 2))
(define (double x)
  (* x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 3 0)
(fast-mult 3 1)
(fast-mult 3 2)
(fast-mult 3 3)
(fast-mult 3 4)
(fast-mult 0 5)
(fast-mult 2 4) 
(fast-mult 4 0) 
(fast-mult 5 1) 
(fast-mult 7 10) 

;; Exercice 1.18
(newline)
(display "Exercice 1.18") (newline)
(define (fast-mult-iter a b)
  (fast-mult-with-acc a b 0))

(define (fast-mult-with-acc a b acc)
  (cond ((= b 0) acc)
        ((even? b) (fast-mult-with-acc (double a) (halve b) acc))
        (else (fast-mult-with-acc a (- b 1) (+ a acc)))))

(fast-mult-iter 3 0)
(fast-mult-iter 3 1)
(fast-mult-iter 3 2)
(fast-mult-iter 3 3)
(fast-mult-iter 3 4)
(fast-mult-iter 0 5)
(fast-mult-iter 2 4) 
(fast-mult-iter 4 0) 
(fast-mult-iter 5 1) 
(fast-mult-iter 7 10)

;; Exercice 1.19
(newline)
(display "Exercice 1.19")(newline)

(define (fib2 n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))      ; compute p'
                   (+ (* 2 p q) (square q))      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib2 0)
(fib2 1)
(fib2 2)
(fib2 3)
(fib2 4)
(fib2 5)
(fib2 6)
(fib2 7)
(fib2 8)
(fib2 9)
(fib2 10)
(fib2 11)
(fib2 12)
(fib2 13)
(fib2 14)
(fib2 15)

;; Exercice 1.21
(newline) (display "Exercice 1.21") (newline)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; Exercice 1.22
(newline) (display "Exercice 1.22") (newline)

(define (timed-prime-test n)
  ;;(display "timed")
  ;;(newline)
  ;;(display "test = ")
  ;;(display n)
  ;;(newline)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes base)
  (define (search-for-next-n-primes base n)
    ;;(display "search for next primes for ") (display base) (display ", n = ") (display n) (newline)
    (cond ((not (= n 0))
           (cond ((prime? base)
                  (timed-prime-test base)
                  (display "prime = ")
                  (display base)
                  (newline)
                  (search-for-next-n-primes (inc base) (dec n)))
                 (else (search-for-next-n-primes (inc base) n))))))
  (search-for-next-n-primes base 3))
(search-for-primes 100)
(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)

;; *** 98
;;prime = 1009
;; *** 3
;;prime = 1013
;; *** 3
;;prime = 1019
;; *** 8
;;prime = 10007
;; *** 8
;;prime = 10009
;; *** 10
;;prime = 10037
;; *** 25
;;prime = 100003
;; *** 58
;;prime = 100019
;; *** 26
;;prime = 100043
;; *** 78
;;prime = 1000003
;; *** 77
;;prime = 1000033
;; *** 112
;;prime = 1000037

;; Exercice 1.23

(define (exercice-1-23)
  (define (next n)
    (if (= n 2)
        (+ n 1)
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (prime? n)
    (= n (smallest-divisor n)))
  
  (define (timed-prime-test n)
    (start-prime-test n (runtime)))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    (newline))
  (define (search-for-primes base)
    (define (search-for-next-n-primes base n)
      (cond ((not (= n 0))
             (cond ((prime? base)
                    (timed-prime-test base)
                    (display "prime = ")
                    (display base)
                    (newline)
                    (search-for-next-n-primes (inc base) (dec n)))
                   (else (search-for-next-n-primes (inc base) n))))))
    (search-for-next-n-primes base 3))
  
  (newline) (display "Exercice 1.23") (newline)
  (search-for-primes 100)
  (search-for-primes 1000)
  (search-for-primes 10000)
  (search-for-primes 100000)
  (search-for-primes 1000000))

(exercice-1-23)

;; Exercice 1.24
(define (exercice-1-24)
  (define (next n)
    (if (= n 2)
        (+ n 1)
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (prime? n)
    (= n (smallest-divisor n)))
  
  (define (timed-prime-test n)
    (start-prime-test n (runtime)))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    (newline))
  (define (search-for-primes base)
    (define (search-for-next-n-primes base n)
      (cond ((not (= n 0))
             (cond ((fast-prime? base 1)
                    (timed-prime-test base)
                    (display "prime = ")
                    (display base)
                    (newline)
                    (search-for-next-n-primes (inc base) (dec n)))
                   (else (search-for-next-n-primes (inc base) n))))))
    (search-for-next-n-primes base 3))
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
  
  (newline)(display "Exercice 1.24")(newline)
  (search-for-primes 100)
  (search-for-primes 1000)
  (search-for-primes 10000)
  (search-for-primes 100000)
  (search-for-primes 1000000))

(exercice-1-24)

;; Exercice 1.25
(define (exercice-1-25)
  (define (next n)
    (if (= n 2)
        (+ n 1)
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (prime? n)
    (= n (smallest-divisor n)))
  
  (define (timed-prime-test n)
    (start-prime-test n (runtime)))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    (newline))
  (define (search-for-primes base)
    (define (search-for-next-n-primes base n)
      (cond ((not (= n 0))
             (cond ((fast-prime? base 1)
                    (timed-prime-test base)
                    (display "prime = ")
                    (display base)
                    (newline)
                    (search-for-next-n-primes (inc base) (dec n)))
                   (else (search-for-next-n-primes (inc base) n))))))
    (search-for-next-n-primes base 3))
  (define (expmod base exp m)
    (remainder (fast-expt base exp) m))        
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  
  (newline)(display "Exercice 1.25")(newline)
  (search-for-primes 100)
  (search-for-primes 1000)
  (search-for-primes 10000)
  (search-for-primes 100000)
  (search-for-primes 1000000))

;;(exercice-1-25)





;; Exercice 1.27
(define (exercice-1-27)
  (define (next n)
    (if (= n 2)
        (+ n 1)
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (prime? n)
    (= n (smallest-divisor n)))
  
  (define (timed-prime-test n)
    (start-prime-test n (runtime)))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
    (newline))
  (define (search-for-primes base)
    (define (search-for-next-n-primes base n)
      (cond ((not (= n 0))
             (cond ((fast-prime? base 1)
                    (timed-prime-test base)
                    (display "prime = ")
                    (display base)
                    (newline)
                    (search-for-next-n-primes (inc base) (dec n)))
                   (else (search-for-next-n-primes (inc base) n))))))
    (search-for-next-n-primes base 3))
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


  (define (test-for-prime n)
    (define (test-for-prime-helper n a)
      (cond ((= a n) #t)
            ((not (= (expmod a n n) a)) #f)
            (else (test-for-prime-helper n (inc a)))))
    (test-for-prime-helper n 1))
  
  (newline)(display "Exercice 1.27")(newline)
  (display (test-for-prime 13))
  (display (test-for-prime 561))
  (display (prime? 561))
  (display (fast-prime? 561 3))

  (display "TEST = ") (display (expmod 3 9 9)) (newline)
  )

(exercice-1-27)




;; Exercice 1.28
(define (exercice-1-28)
  (define (expmod base exp m)
    (define (square-non-trivial-check-root a)
      (display "square ") (display a) (newline)
      (cond ((or (= 1 a) (= (dec m) a)) (display "bis") (newline) (square a))
            ((= (remainder (square a) m) 1) (display "Found ") (display a) (newline) 0)
            (else  (display "else") (newline) (square a))))
    (display "expmod ") (display base) (display " ") (display exp) (display " ") (display m) (newline)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder (square-non-trivial-check-root (expmod base (/ exp 2) m))
                      m))
          (else
           (remainder (* base (expmod base (- exp 1) m))
                      m))))
  
  (define (fermat-test n)
    (define (try-it a)
      (display "Try with ") (display a) (display " for n = ") (display n) (newline)
      (not (= (expmod a (dec n) n) 0)))
    (try-it (+ 1 (random (- n 1)))))

  (define (fast-prime? n)
    (define (iter times)
      (cond ((= times 0) true)
            ((fermat-test n) (iter (- times 1)))
            (else false)))
    (iter 10))
 
  (newline)(display "Exercice 1.28")(newline)
  (display (fast-prime? 2)) (newline)
  (display (fast-prime? 3)) (newline)
  (display (fast-prime? 4)) (newline)
  (display (fast-prime? 5)) (newline)
  ;;(display (fast-prime? 6)) (newline)

  (display (fast-prime? 9)) (newline)
(expmod 3 9 9)

  )



(exercice-1-28)