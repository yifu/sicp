#lang sicp

(define (exercise-2-1)
  (define (mk-rat n d)
    (let ((g (gcd n d)))
      (let ((n (/ n g))
            (d (/ d g)))
        (if (< d 0)
            (cons (- n) (- d))
            (cons n d)))))

  (define (numer r) (car r))

  (define (denom r) (cdr r))

  (define (print-rat r)
    (display (numer r))
    (display " / ")
    (display (denom r)))

  (display "exercise 2.1") (newline)
  (print-rat (mk-rat 2 4)) (newline)
  (print-rat (mk-rat -2 4)) (newline)
  (print-rat (mk-rat 2 -4)) (newline)
  (print-rat (mk-rat -2 -4)) (newline)
  (print-rat (mk-rat 1 -2)) (newline)
  (print-rat (mk-rat 6 -9)) (newline))
(exercise-2-1)
