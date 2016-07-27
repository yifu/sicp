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