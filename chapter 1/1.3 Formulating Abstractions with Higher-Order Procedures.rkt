#lang sicp

;; Exercise 1.29
;; Simpson's Rule Integral
(define (exercise-1-29)
  (define (identity x) x)
  (define (cube x) (* x x x))

  (define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))

  (define (simpson-rule-integral f a b n)
    (define h
      (/ (- b a) n))
    (define (term k)
      (* (f (+ a (* k h)))
         (cond ((= k 0) 1)
               ((= k n) 1)
               ((even? k) 2)
               (else 4))))
    (* (/ h 3) (sum term 0 inc n)))

  (newline) (display "Exercise 1.29") (newline)

  ;(sum identity 0 inc 10)
  (display "Test cases for Simpson's Integral - recursive version:") (newline)
  (display (simpson-rule-integral cube 0 1 1)) (newline)
  (display (simpson-rule-integral cube 0 1 10)) (newline)
  (display (simpson-rule-integral cube 0 1 100)) (newline)
  (display (simpson-rule-integral cube 0 1 1000)) (newline))

(exercise-1-29)

;; Exercice 1.30
;; Iterative version of sum
(define (exercice-1-30)
  (define (itersum term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (+ (term a) result))))
    (iter a 0))

  (define (cube x) (* x x x))
  (define (simpson-rule-integral f a b n)
    (define h
      (/ (- b a) n))
    (define (term k)
      (* (f (+ a (* k h)))
         (cond ((= k 0) 1)
               ((= k n) 1)
               ((even? k) 2)
               (else 4))))
    (* (/ h 3) (itersum term 0 inc n)))

  (newline) (display "Exercice 1.30") (newline)

  ;; Test cases for itersum:
  (display "Test cases for Simpson's Integral - iterative version:") (newline)
  (display (simpson-rule-integral cube 0 1 1)) (newline)
  (display (simpson-rule-integral cube 0 1 10)) (newline)
  (display (simpson-rule-integral cube 0 1 100)) (newline)
  (display (simpson-rule-integral cube 0 1 1000)) (newline))

(exercice-1-30)

;; Exercise 1.31
(define (exercise-1-31)
  (define (identity x) x)
  (define (square x) (* x x))
  (define (cube x) (* x x x))

  (define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))

  (define (product-iter term a next b)
    (define (iter acc a b)
      (if (> a b)
          acc
          (iter (* (term a) acc) (next a) b)))
    (iter 1 a b))

  (define (factorial n)
    (product identity 1 inc n))

  (define (factorial-iter n)
    (product-iter identity 1 inc n))

  (define (pi-approx lvl)
    (define (term n)
      (define (num-term n) (if (even? n) (+ n 2) (+ n 3)))
      (define (den-term n) (if (even? n) (+ n 3) (+ n 2)))
      (/ (num-term n) (den-term n)))
    (* 4 (product term 0 inc lvl)))

  (define (pi-approx-iter lvl)
    (define (term n)
      (define (num-term n) (if (even? n) (+ n 2) (+ n 3)))
      (define (den-term n) (if (even? n) (+ n 3) (+ n 2)))
      (/ (num-term n) (den-term n)))
    (* 4 (product-iter term 0 inc lvl)))

  (newline) (display "Exercise 1.31") (newline)
  (display "Test Factorial") (newline)
  (display (factorial 1)) (newline)
  (display (factorial 2)) (newline)
  (display (factorial 3)) (newline)
  (display (factorial 4)) (newline)
  (display "Test Iterative Factorial") (newline)
  (display (factorial-iter 1)) (newline)
  (display (factorial-iter 2)) (newline)
  (display (factorial-iter 3)) (newline)
  (display (factorial-iter 4)) (newline)

  (display "Test Pi approximation") (newline)
  ;(display (term 0)) (newline)
  ;(display (term 1)) (newline)
  ;(display (term 2)) (newline)
  ;(display (term 3)) (newline)
  ;(display (term 4)) (newline)
  ;(display (term 5)) (newline)
  (display (pi-approx 5)) (newline)
  (display (pi-approx 100)) (newline)

  (display "Test Iterative Pi approximation ") (newline)
  (display (pi-approx-iter 5)) (newline)
  (display (pi-approx-iter 100)) (newline))

(exercise-1-31)

;; Exercise 1.32
(define (exercise-1-32)
  (define (identity x) x)
  (define (square x) (* x x))
  (define (cube x) (* x x x))

  (define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))

  (define (accumulate-iter combiner null-value term a next b)
    (define (iter a b acc)
      (if (> a b)
          acc
          (iter (next a) b (combiner acc (term a)))))
    (iter a b null-value))

  (define (sum term a next b)
    (accumulate + 0 term a next b))

  (define (sum-iter term a next b)
    (accumulate-iter + 0 term a next b))

  (define (simpson-rule-integral f a b n)
    (define h
      (/ (- b a) n))
    (define (term k)
      (* (f (+ a (* k h)))
         (cond ((= k 0) 1)
               ((= k n) 1)
               ((even? k) 2)
               (else 4))))
    (* (/ h 3) (sum term 0 inc n)))

  (define (simpson-rule-integral-iter f a b n)
    (define h
      (/ (- b a) n))
    (define (term k)
      (* (f (+ a (* k h)))
         (cond ((= k 0) 1)
               ((= k n) 1)
               ((even? k) 2)
               (else 4))))
    (* (/ h 3) (sum-iter term 0 inc n)))

  (define (product term a next b)
    (accumulate * 1 term a next b))

  (define (product-iter term a next b)
    (accumulate-iter * 1 term a next b))

  (define (factorial n)
    (product identity 1 inc n))

  (define (factorial-iter n)
    (product-iter identity 1 inc n))

  (newline) (display "Exercise 1.32") (newline)
  (display "Test cases for Simpson's Integral - recursive version:") (newline)
  (display (simpson-rule-integral cube 0 1 1)) (newline)
  (display (simpson-rule-integral cube 0 1 10)) (newline)
  (display (simpson-rule-integral cube 0 1 100)) (newline)
  (display (simpson-rule-integral cube 0 1 1000)) (newline)
  (display "Test cases for Simpson's Integral - iterative version:") (newline)
  (display (simpson-rule-integral-iter cube 0 1 1)) (newline)
  (display (simpson-rule-integral-iter cube 0 1 10)) (newline)
  (display (simpson-rule-integral-iter cube 0 1 100)) (newline)
  (display (simpson-rule-integral-iter cube 0 1 1000)) (newline)

  (display "Test Factorial - recursive version:") (newline)
  (display (factorial-iter 1)) (newline)
  (display (factorial-iter 2)) (newline)
  (display (factorial-iter 3)) (newline)
  (display (factorial-iter 4)) (newline)

  (display "Test Factorial - iterative version:") (newline)
  (display (factorial 1)) (newline)
  (display (factorial 2)) (newline)
  (display (factorial 3)) (newline)
  (display (factorial 4)) (newline))

(exercise-1-32)

;; Exercise 1.33
(#%require math/number-theory)
(define (exercise-1-33)
  (define (square n) (* n n))
  ;(define (identity n) n)

  (define (filtered-accumulate combiner null-value term a next b filter)
    (if (> a b)
        null-value
        (combiner (if (filter a) (term a) null-value) (filtered-accumulate combiner null-value term (next a) next b filter))))

  (define (sum-square-prime a b)
   (filtered-accumulate + 0 square a inc b prime?))

  (define (product-coprimes-n n)
    (define (coprime-n? x) (coprime? x n))
    (filtered-accumulate * 1 identity 1 inc (dec n) coprime-n?))

  (newline) (display "Exercise 1.33") (newline)
  (display "Compute the sum of squares of prime integers between 0 and 10:") (newline)
  (display (sum-square-prime 0 10)) (newline)
  (display "Compute the product of the coprimes of 5") (newline)
  (display (product-coprimes-n 5)) (newline))

(exercise-1-33)

;; Exercise 1.34
(define (exercise-1-34)
  (define (square n) (* n n))

  (define (f g)
    (g 2))

  (newline) (display "Exercise 1.34") (newline)
  (display (f square)) (newline)
  (display (f (lambda (x) (* x (+ x 1))))) (newline)
;  (f f)
  )

;; Exercise 1.35
(define (exercise-1-35)
  (define (square n) (* n n))
  (define tolerance 0.00001)
  (define (fixed-point f first-guess)
    (define (close-enough? a b)
      (< (abs (- a b)) tolerance))
    (define (try guess)
      (let ((next-guess (f guess)))
        (if (close-enough? guess next-guess)
            next-guess
            (try next-guess))))
    (try first-guess))

  (newline) (display "Exercise 1.35") (newline)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))
(exercise-1-35)

;; Exercise 1.36
(define (exercise-1-36)
  (define (square n) (* n n))
  (define tolerance 0.00001)
  (define (fixed-point f first-guess)
    (define (close-enough? a b)
      (< (abs (- a b)) tolerance))
    (define (try guess)
      (display "guess = ") (display guess) (newline)
      (let ((next-guess (f guess)))
        (if (close-enough? guess next-guess)
            next-guess
            (try next-guess))))
    (try first-guess))

  (newline) (display "Exercise 1.36") (newline)
  (display "Fixed point without dampening") (newline)
  (display (fixed-point (lambda (x) (/ (log 1000) (log x))) 2)) (newline)

  (let ((result (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)))
    (display "Fixed point with dampening") (newline)
    (display "result = ") (display result) (newline)
    (display "verif = ") (display (expt result result)) (newline)))
(exercise-1-36)

;; Exercise 1.37
(define (exercise-1-37)
;  (define (cont-frac n d k)
;    (if (= k 1)
;        (/ (n 1) (d 1))
;        (/ (n k) (+ (d k) (cont-frac n d (dec k))))))
  (define (cont-frac n d k)
    (define (recur i)
      (if (> i k)
          0
          (/ (n i) (+ (d i) (recur (inc i))))))
    (recur 1))

  (define (cont-frac-iter n d k)
    (define (iter k acc)
      (if (= k 1)
          (/ (n 1) (+ (d 1) acc))
          (iter (dec k) (/ (n k) (+ (d k) acc)))))
    (iter k 0))

  (newline) (display "Exercise 1.37") (newline)
  (display "Testing recursive process") (newline)
  (display "k = 1, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1)) (newline)
  (display "k = 2, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 2)) (newline)
  (display "k = 3, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3)) (newline)
  (display "k = 4, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 4)) (newline)
  (display "k = 5, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)) (newline)
  (display "k = 6, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 6)) (newline)
  (display "k = 7, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 7)) (newline)
  (display "k = 8, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)) (newline)
  (display "k = 9, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 9)) (newline)
  (display "k = 10, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)) (newline)
  (display "k = 11, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)) (newline)
  (display "k = 12, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)) (newline)
  (display "k = 13, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 13)) (newline)
  (display "k = 14, result = ") (display (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 14)) (newline)
  (display "Testing iterative process") (newline)
  (display "k = 1, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 1)) (newline)
  (display "k = 2, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 2)) (newline)
  (display "k = 3, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 3)) (newline)
  (display "k = 4, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 4)) (newline)
  (display "k = 5, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 5)) (newline)
  (display "k = 6, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 6)) (newline)
  (display "k = 7, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 7)) (newline)
  (display "k = 8, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 8)) (newline)
  (display "k = 9, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 9)) (newline)
  (display "k = 10, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)) (newline)
  (display "k = 11, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)) (newline)
  (display "k = 12, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 12)) (newline)
  (display "k = 13, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 13)) (newline)
  (display "k = 14, result = ") (display (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 14)) (newline))
(exercise-1-37)

;; Exercise 1.38
(define (exercise-1-38)
  (define (cont-frac n d k)
    (define (recur i)
      (if (> i k)
          0
          (/ (n i) (+ (d i) (recur (inc i))))))
    (recur 1))

  (define (cont-frac-iter n d k)
    (define (iter k acc)
      (if (= k 1)
          (/ (n 1) (+ (d 1) acc))
          (iter (dec k) (/ (n k) (+ (d k) acc)))))
    (iter k 0))

  (define (n k) 1.0)
  (define (d k)
    (if (= (remainder k 3) 2)
        (* 2/3 (/ (inc k) 1))
        1))

  (newline) (display "Exercise 1.38") (newline)
  (display (cont-frac-iter n d 1)) (newline)
  (display (cont-frac-iter n d 2)) (newline)
  (display (cont-frac-iter n d 3)) (newline)
  (display (cont-frac-iter n d 4)) (newline)
  (display (cont-frac-iter n d 5)) (newline)
  (display (cont-frac-iter n d 6)) (newline)
  (display (cont-frac-iter n d 7)) (newline)
  (display (cont-frac-iter n d 8)) (newline)
  (display (cont-frac-iter n d 9)) (newline)
  (display (cont-frac-iter n d 10)) (newline))
(exercise-1-38)

;; Exercise 1.39
(define (exercise-1-39)
  (define (cont-frac n d k)
    (define (recur i)
      (if (> i k)
          0
          (/ (n i) (+ (d i) (recur (inc i))))))
    (recur 1))

  (define (cont-frac-iter n d k)
    (define (iter k acc)
      (if (= k 1)
          (/ (n 1) (+ (d 1) acc))
          (iter (dec k) (/ (n k) (+ (d k) acc)))))
    (iter k 0))

  (define (tan-cf x k)
    (define (n i)
      (if (= i 1)
          x
          (- (* x x))))
    (define (d i)
      (dec (* i 2)))
    (cont-frac-iter n d k))

  (newline) (display "Exercise 1.39") (newline))
(exercise-1-39)

;; Exercise 1.40
(define (exercise-1-40)
  (define tolerance 0.00001)
  (define (fixed-point f first-guess)
    (define (close-enough? a b)
      (< (abs (- a b)) tolerance))
    (define (try guess)
      (display "guess = ") (display guess) (newline)
      (let ((next-guess (f guess)))
        (if (close-enough? guess next-guess)
            next-guess
            (try next-guess))))
    (try first-guess))

  (define dx 0.00001)

  (define (deriv f)
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx)))

  (define (newton-transform f)
    (lambda (x) (- x (/ (f x) ((deriv f) x)))))

  (define (newton-method f first-guess)
    (fixed-point (newton-transform f) first-guess))

  (define (cubic a b c)
    (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

  (newline) (display "Exercise 1.40") (newline)
  ;((newton-transform  (cubic 1 1 1)) 1.0)
  (display (newton-method (cubic 0 0 0) 1.0)) (newline))
(exercise-1-40)

;; Exercise 1.41
(define (exercise-1-41)

;  (define (plus1 x)
;    (display "here") (newline)
;    (inc x))

  (define (double f)
    (lambda (x)
      (f (f x))))

  (newline) (display "Exercise 1.41") (newline)
  ;(display ((double inc) 5)) (newline)
  ;(display (((double double) inc) 5)) (newline)
  ;(display ((double (double (double inc))) 5)) (newline)
  (display (((double (double double)) inc) 5)) (newline))
(exercise-1-41)

;; Exercise 1.42
(define (exercise-1-42)

  (define (square x) (* x x))

  (define (compose f g)
    (lambda (x) (f (g x))))

  (newline) (display "Exercise 1.42") (newline)
  (display ((compose square inc) 6)) (newline))
(exercise-1-42)

;; Exercise 1.43
(define (exercise-1-43)

  (define (square x) (* x x))

  (define (compose f g)
    (lambda (x) (f (g x))))

  (define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated f (dec n)))))

  (newline) (display "Exercise 1.43") (newline)
  (display ((repeated square 2) 5)) (newline))
(exercise-1-43)

;; Exercise 1.44
(define (exercise-1-44)

  (define (square x) (* x x))

  (define (compose f g)
    (lambda (x) (f (g x))))

  (define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated f (dec n)))))

  (define (smooth f)
    (define (average a b c)
      (/ (+ a b c) 3))
    (define dx 0.00001)
    (lambda (x)
      (average (f (- x dx))
               (f x)
               (f (+ x dx)))))

  (newline) (display "Exercise 1.44") (newline)
  (display (((repeated smooth 10) square) 5)) (newline))
(exercise-1-44)

;; Exercise 1.45
(define (exercise-1-45)
  (define (square n) (* n n))
  (define (cube n) (* n n n))
  (define (avg a b) (/ (+ a b) 2))

  (define (avg-damp f)
    (lambda (x) (avg x (f x))))

  (define tolerance 0.00001)
  (define (fixed-point f first-guess)
    (define (close-enough? a b)
      (< (abs (- a b)) tolerance))
    (define (try guess)
      (let ((next-guess (f guess)))
        (if (close-enough? guess next-guess)
            next-guess
            (try next-guess))))
    (try first-guess))

  (define (compose f g)
    (lambda (x) (f (g x))))

  (define (repeated f n)
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeated f (dec n)))))

  (define (n-root y n)
    (cond ((= n 1) y)
          ((= n 2) (fixed-point (avg-damp (lambda (x) (/ y (expt x (dec n))))) 1.0))
          (else (fixed-point ((repeated avg-damp (- n 2)) (lambda (x) (/ y (expt x (dec n))))) 1.0))))

  (newline) (display "Exercise 1.45") (newline)
  (display (fixed-point (lambda (x) (avg x (/ 9 x))) 1.0)) (newline)
  (display (fixed-point (lambda (x) (avg x (/ 27 (square x)))) 1.0)) (newline)
  (display (fixed-point (lambda (x) (avg x (avg x (/ 81 (cube x))))) 1.0)) (newline)
  (display (fixed-point (lambda (x) (avg x (avg x (avg x (/ 243 (expt x 4)))))) 1.0)) (newline)
  (display (fixed-point (lambda (x) (avg x (avg x (avg x (avg x (/ 729 (expt x 5))))))) 1.0)) (newline)
  (display "Test n-root:") (newline)
  (display (n-root 9 2)) (newline)
  (display (n-root 27 3)) (newline)
  (display (n-root 81 4)) (newline)
  (display (n-root 243 5)) (newline)
  (display (n-root 729 6)) (newline))
(exercise-1-45)
