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
  (print-rat (mk-rat 6 -9)) (newline)
  (newline))
(exercise-2-1)

(define (exercise-2-2)
  (define (make-segment start end) (cons start end))
  (define (start-segment s) (car s))
  (define (end-segment s) (cdr s))

  (define (make-point x y) (cons x y))
  (define (x-point p) (car p))
  (define (y-point p) (cdr p))
  (define (print-point p)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))

  (define (midpoint-segment s)
    (define (avg a b) (/ (+ a b) 2))
    (make-point (avg (x-point (start-segment s)) (x-point (end-segment s)))
                (avg (y-point (start-segment s)) (y-point (end-segment s)))))

  (display "exercise 2.2") (newline)
  (print-point (make-point 1 2)) (newline)
  (print-point (midpoint-segment (make-segment (make-point 1 1) (make-point 2 2)))) (newline)
  (newline))
(exercise-2-2)

(define (exercise-2-3)
  (define (square x) (* x x))

  (define (make-point x y) (cons x y))
  (define (x-point p) (car p))
  (define (y-point p) (cdr p))
  (define (print-point p)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))

  (define (make-segment start end) (cons start end))
  (define (start-segment s) (car s))
  (define (end-segment s) (cdr s))

  (define (length-segment s)
    (let ((start (start-segment s))
          (end (end-segment s)))
      (sqrt (+ (square (- (x-point end) (x-point start)))
               (square (- (y-point end) (y-point start)))))))

  (define (midpoint-segment s)
    (define (avg a b) (/ (+ a b) 2))
    (make-point (avg (x-point (start-segment s)) (x-point (end-segment s)))
                (avg (y-point (start-segment s)) (y-point (end-segment s)))))

  (define (make-rectangle length width) (cons length width))
  (define (length-rectangle r) (car r))
  (define (width-rectangle r) (cdr r))

  (define (perimeter-rectangle r)
    (+ (* (length-segment (length-rectangle r)) 2)
       (* (length-segment (width-rectangle r)) 2)))
  (define (area-rectangle r)
    (* (length-segment (length-rectangle r)) (length-segment (width-rectangle r))))

  (display "exercise 2.3") (newline)
  (display "Testing length-segment:") (newline)
  (display (length-segment (make-segment (make-point 0 0) (make-point 1 0)))) (newline)
  (display (length-segment (make-segment (make-point 0 0) (make-point 0 1)))) (newline)
  (display (length-segment (make-segment (make-point 0 0) (make-point 1 1)))) (newline)
  (display "Testing perimeter and area of a rectangle:") (newline)
  (let ((rect (make-rectangle (make-segment (make-point 0 0) (make-point 0 1))
                              (make-segment (make-point 0 0) (make-point 1 0)))))
    (display (perimeter-rectangle rect)) (newline)
    (display (area-rectangle rect)) (newline))
  (newline))
(exercise-2-3)

(define (exercise-2-4)
  (define (cons x y)
    (lambda (m) (m x y)))

  (define (car p)
    (p (lambda (x y) x)))

  (define (cdr p)
    (p (lambda (x y) y)))

  (display "exercise 2.4") (newline)
  (display "(car (cons 1 2)) = ") (display (car (cons 1 2))) (newline)
  (display "(cdr (cons 1 2)) = ") (display (cdr (cons 1 2))) (newline)
  (newline))
(exercise-2-4)

(define (exercise-2-5)
  (define (cons x y)
    (* (expt 2 x) (expt 3 y)))

  (define (car p)
    (if (not (= (remainder p 2) 0))
        0
        (inc (car (/ p 2)))))

  (define (cdr p)
    (if (not (= (remainder p 3) 0))
        0
        (inc (cdr (/ p 3)))))

  (display "exercise 2.5") (newline)
  (display "(car (cons 1 2)) = ") (display (car (cons 1 2))) (newline)
  (display "(cdr (cons 1 2)) = ") (display (cdr (cons 1 2))) (newline)

  (display "(car (cons 5 20)) = ") (display (car (cons 5 20))) (newline)
  (display "(cdr (cons 5 20)) = ") (display (cdr (cons 5 20))) (newline)

  (display "(car (cons 10 6)) = ") (display (car (cons 10 6))) (newline)
  (display "(cdr (cons 10 6)) = ") (display (cdr (cons 10 6))) (newline)

  (display "(car (cons 0 0)) = ") (display (car (cons 0 0))) (newline)
  (display "(cdr (cons 0 0)) = ") (display (cdr (cons 0 0))) (newline)

  (display "(car (cons 0 1)) = ") (display (car (cons 0 1))) (newline)
  (display "(cdr (cons 0 1)) = ") (display (cdr (cons 0 1))) (newline)

  (display "(car (cons 1 0)) = ") (display (car (cons 1 0))) (newline)
  (display "(cdr (cons 1 0)) = ") (display (cdr (cons 1 0))) (newline)
  (newline))
(exercise-2-5)
