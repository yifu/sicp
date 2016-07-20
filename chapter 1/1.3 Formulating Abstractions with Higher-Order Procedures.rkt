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

    (display "h = ") (display h) (newline)
    (* (/ h 3) (sum term 0 inc n)))

  (newline) (display "Exercise 1.29") (newline)

  ;(sum identity 0 inc 10)

  (display (simpson-rule-integral cube 0 1 1)) (newline)
  (display (simpson-rule-integral cube 0 1 10)) (newline)
  (display (simpson-rule-integral cube 0 1 100)) (newline)
  (display (simpson-rule-integral cube 0 1 1000)) (newline))

(exercise-1-29)

;; Exercice 1.30
;; Iterative version of sum
(define (exercice-1-30)
  (define (sum term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (iter (next a) (+ (term a) result))))
    (iter a 0))

  (newline) (display "Exercice 1.30") (newline))

(exercice-1-30)