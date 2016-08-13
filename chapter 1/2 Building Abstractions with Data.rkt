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

(define (exercise-2-6)
  (define zero (lambda (f) (lambda (x) x)))
  (define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

  ;;(define one (lambda (f) (lambda (x) (f (f x)))))
  ;;(define two (lambda (f) (lambda (x) (f (lambda (x) (f (f f))) x))))

  ;(add-1 zero)
  ;(lambda (f) (lambda (x) (f ((zero f) x))))
  ;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
  ;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
  ;(lambda (f) (lambda (x) (f x)))
  (define one (lambda (f) (lambda (x) (f x))))

  ;(add-1 one)
  ;((lambda (f) (lambda (x) (f ((n f) x)))) one)
  ;(lambda (f) (lambda (x) (f ((one f) x))))
  ;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
  ;(lambda (f) (lambda (x) (f (f x))))
  (define two (lambda (f) (lambda (x) (f (f x)))))

  ;; ...
  (define three (lambda (f) (lambda (x) (f (f (f x))))))

  ;(define three-v2 (lambda (f) (lambda (x) ((two f) ((one f) x)))))
  ;(lambda (f) (lambda (x) ((two f) ((one f) x))))
  ;(lambda (f) (lambda (x) ((two f) (((lambda (f1) (lambda (x1) (f1 x1))) f) x))))
  ;(lambda (f) (lambda (x) ((two f) ((lambda (x1) (f x1)) x))))
  ;(lambda (f) (lambda (x) ((two f) (f x))))
  ;(lambda (f) (lambda (x) ((lambda (x1) (f (f x1))) (f x))))
  ;(lambda (f) (lambda (x) (f (f (f x)))))
  (define three-v2 (lambda (f) (lambda (x) ((two f) ((one f) x)))))

  ; (plus a b) is somehow a generalisation of (add-1 n).
  ; It 'applies' f to parameters, 'applies' x to the second paramater,
  ; and this result is 'applied' to the first parameter.
  (define (plus a b)
    (lambda (f) (lambda (x) ((a f) ((b f) x)))))

  (define (square x) (* x x))

  (display "exercise 2.6") (newline)
  ;(display "test = ") (display (add-1 zero)) (newline)
  ;(display "plus = ") (display (plus two one)) (newline)
  (display "test = ") (display ((three square) 3)) (newline)
  (display "verif = ") (display (square (square (square 3)))) (newline)
  (display "test three-v2 = ") (display ((three-v2 square) 3)) (newline)
  (display "test plus = ") (display (((plus two one) square) 3)) (newline)
  (newline))
(exercise-2-6)

(define (exercise-2-7)
  (define (make-interval a b) (cons a b))
  (define (lower-bound interval) (car interval))
  (define (upper-bound interval) (cdr interval))
  (display "exercise 2.7") (newline)
  (newline))
(exercise-2-7)

(define (exercise-2-8)
  (define (make-interval a b) (cons a b))
  (define (lower-bound interval) (car interval))
  (define (upper-bound interval) (cdr interval))

  (define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound y) (upper-bound y))))

  (define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))
  (display "exercise 2.8") (newline)
  (newline))
(exercise-2-8)

(define (exercise-2-10)
  (define (make-interval a b) (cons a b))
  (define (lower-bound interval) (car interval))
  (define (upper-bound interval) (cdr interval))

  (define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound y) (upper-bound y))))

  (define (sub-interval x y)
    (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

  (define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

  (define (div-interval x y)
    (define (span-over-0-interval? x)
      (and (< (lower-bound x) 0) (> (upper-bound x) 0)))

    (if (span-over-0-interval? y)
        (display "div-interval cannot divide by an interval which spans over 0.")
        (mul-interval x (make-interval (/ 1 (upper-bound y)) (/ 1 (lower-bound y))))))

  (display "exercise 2.10") (newline)
  (newline))
(exercise-2-10)

(define (exercise-2-11)

  (display "exercise 2.11") (newline)
  (newline))
(exercise-2-11)
