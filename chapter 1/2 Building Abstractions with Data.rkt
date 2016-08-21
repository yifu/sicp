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

(define (exercise-2-12)
  (define (make-interval a b) (cons a b))
  (define (lower-bound interval) (car interval))
  (define (upper-bound interval) (cdr interval))

  (define (make-center-width c w)
    (make-interval (- c w) (+ c w)))
  (define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
  (define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

  (define (make-center-percent center percent)
    (let ((w (/ (* center percent) 100)))
      (let ((lower (- center w))
            (upper (+ center w)))
        (make-interval lower upper))))

  (define (percent i)
    (let ((w (width i))
          (c (center i)))
      (/ (* (/ w 2) 100) c)))

  (display "exercise 2.12") (newline)
  (newline))
(exercise-2-12)


(define (exercise-2-14)
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

  (define (make-center-width c w)
    (make-interval (- c w) (+ c w)))
  (define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))
  (define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

  (define (make-center-percent center percent)
    (let ((w (/ (* center percent) 100)))
      (let ((lower (- center w))
            (upper (+ center w)))
        (make-interval lower upper))))

  (define (percent i)
    (let ((w (width i))
          (c (center i)))
      (/ (* (/ w 2) 100) c)))

  (define (print-interval i)
    (display "(")
    (display (lower-bound i))
    (display ", ")
    (display (upper-bound i))
    (display ")"))

  (define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

  (define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval one
                    (add-interval (div-interval one r1)
                                  (div-interval one r2)))))

  (display "exercise 2.14") (newline)
  (print-interval (make-interval 1 1)) (newline)
  (print-interval (div-interval (make-interval 13 13.25) (make-interval 13 13.25))) (newline)
  (print-interval (par1 (make-interval 13 13.25) (make-interval 16 16.25))) (newline)
  (print-interval (par2 (make-interval 13 13.25) (make-interval 16 16.25))) (newline)
  (newline))
(exercise-2-14)

(define (exercise-2-17)
  (define (last-pair l)
    (if (null? (cdr l))
        l
        (last-pair (cdr l))))

  (display "exercise 2.17") (newline)
  (display "test:") (display (last-pair (list 1 2 3 4))) (newline)
  (newline))
(exercise-2-17)

(define (exercise-2-18)
  (define (reverse l)
    (define (iter l acc)
      (if (null? l)
          acc
          (iter (cdr l) (cons (car l) acc))))
    (iter l nil))

  (display "exercise 2.18") (newline)
  (display "test: ") (display (reverse (list 1 2 3 4))) (newline)
  (newline))
(exercise-2-18)

(define (exercise-2-19)
  (define (reverse l)
    (define (iter l acc)
      (if (null? l)
          acc
          (iter (cdr l) (cons (car l) acc))))
    (iter l nil))

  (define (no-more? l) (null? l))
  (define (except-first-denomination l) (cdr l))
  (define (first-denomination l) (car l))

  (define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
           (+ (cc amount (except-first-denomination coin-values))
              (cc (- amount (first-denomination coin-values)) coin-values)))))

  (define us-coins (list 50 25 10 5 1))
  ;(define us-coins (list 50 1 10 5 25))
  (define uk-coins (list 100 50 20 10 5 2 1 0.5))

  (display "exercise 2.19") (newline)
  (display "cc 100 us-coins: ") (display (cc 100 us-coins)) (newline)
  (display "cc 100 uk-coins: ") (display (cc 100 uk-coins)) (newline)
  (newline))
(exercise-2-19)

(define (exercise-2-20)
  (define (same-parity . l)
    (define (even? n)
      (= (remainder n 2) 0))
    (define (odd? n)
      (not (even? n)))
    (define (filter test? l)
      (cond ((null? l) nil)
            ((test? (car l)) (cons (car l) (filter test? (cdr l))))
            (else (filter test? (cdr l)))))
    (cond ((null? l) nil)
          ((even? (car l)) (filter even? l))
          (else (filter odd? l))))

  (display "exercise 2.20") (newline)
  (display "test 2 3 4 5 6 7: ") (display (same-parity 2 3 4 5 6 7)) (newline)
  (display "test 3 4 5 6 7: ") (display (same-parity 3 4 5 6 7)) (newline)
  (newline))
(exercise-2-20)

(define (exercise-2-21)

  (define (square x) (* x x))

  (define (square-list-1 l)
    (if (null? l)
        nil
        (cons (square (car l)) (square-list-1 (cdr l)))))

  (define (square-list-2 l)
    (map square l))

  (display "exercise 2.21") (newline)
  (display "test (square-list-1 (list 1 2 3 4)): ") (display (square-list-1 (list 1 2 3 4))) (newline)
  (display "test (square-list-2 (list 1 2 3 4)): ") (display (square-list-2 (list 1 2 3 4))) (newline)
  (newline))
(exercise-2-21)

(define (exercise-2-23)

  (define (for-each f l)
    (map f l))

  (display "exercise 2.23") (newline)
  (display "test (for-each (lambda (x) (newline) (display x)) (list 1 2 3 4)): ")

  (for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))
  ;(for-each (lambda (x) (newline) (display x)) nil)

  (newline)
  (newline))
(exercise-2-23)

(define (exercise-2-25)

  (display "exercise 2.25") (newline)
  (display (cadr (caddr (list 1 3 (list 5 7) 9)))) (newline)
  (display (caar (list (list 7)))) (newline)
  (display (cadr (cadr (cadr (cadr (cadr (cadr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))) (newline)
  (newline))
(exercise-2-25)

(define (exercise-2-27)
  (define (reverse l)
    (define (iter l acc)
      (if (null? l)
          acc
          (iter (cdr l) (cons (car l) acc))))
    (iter l nil))

  (define (deep-reverse tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) tree)
          (else (map deep-reverse (reverse tree)))))

  (display "exercise 2.27") (newline)
  (display (deep-reverse (list (list 1 2) (list 3 4)))) (newline)
  (display (deep-reverse (list (list 1 (list (list 23 45) (list 78 (list 1 2 3 4)) 6 (list 6 7 8 9 10))) (list 3 4)))) (newline)
  (newline))
(exercise-2-27)

(define (exercise-2-28)
  (define (reduce f l acc)
    (if (null? l)
        acc
        (reduce f (cdr l) (f acc (car l)))))

  (define (fringe tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else (reduce append (map fringe tree) nil))))

  (display "exercise 2.28") (newline)
  (display (reduce append (list (list 1 2) (list 3 4)) nil)) (newline)
  (display (fringe (list (list 1 2) (list 3 4)))) (newline)
  (display (fringe (list 1 2 3 4))) (newline)
  (display (fringe (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))) (newline)
  (newline))
(exercise-2-28)

(define (exercise-2-29)
  (define (make-mobile left right)
    (list left right))

  (define (make-branch length structure)
    (list length structure))

  (define (left-branch mobile)
    (car mobile))
  (define (right-branch mobile)
    (cadr mobile))
  (define (branch-length branch)
    (car branch))
  (define (branch-structure branch)
    (cadr branch))

  (define (total-branch-weight branch)
    (if (number? (branch-structure branch))
        (branch-structure branch)
        (total-weight (branch-structure branch))))

  (define (total-weight mobile)
    (+ (total-branch-weight (left-branch mobile))
       (total-branch-weight (right-branch mobile))))

  (define (balanced? mobile)
    (define (torque branch)
      (* (branch-length branch) (total-branch-weight branch)))
    (let ((left-torque (torque (left-branch mobile)))
          (right-torque (torque (right-branch mobile))))
      ;(display "lt = ") (display left-torque) (newline)
      ;(display "rt = ") (display right-torque) (newline)
      (and (= left-torque right-torque)
           (or (number? (branch-structure (left-branch mobile))) (balanced? (branch-structure (left-branch mobile))))
           (or (number? (branch-structure (right-branch mobile))) (balanced? (branch-structure (right-branch mobile)))))))

  (display "exercise 2.29") (newline)

  (define x (make-mobile (make-branch 3 4)
                         (make-branch 3 (make-mobile (make-branch 4 5)
                                                     (make-branch 6 7)))))

  (define y (make-mobile (make-branch 1 3)
                         (make-branch 1 2)))

  (define un-balanced-mobile (make-mobile (make-branch 1 10)
                                          (make-branch 2 (make-mobile (make-branch 1 3)
                                                                      (make-branch 1 2)))))

  (define balanced-mobile (make-mobile (make-branch 1 12)
                                       (make-branch 2 (make-mobile (make-branch 1 4)
                                                                   (make-branch 2 2)))))

  (display "total weigth ") (display x) (display " = ") (display (total-weight x)) (newline)
  (display "total weight ") (display y) (display " = ") (display (total-weight y)) (newline)
  (display "balanced? ") (display x) (display ": ") (display (balanced? x)) (newline)
  (display "balanced? ") (display un-balanced-mobile) (display ": ") (display (balanced? un-balanced-mobile)) (newline)
  (display "balanced? ") (display balanced-mobile) (display ": ") (display (balanced? balanced-mobile)) (newline)
  (newline))
(exercise-2-29)

(define (exercise-2-29-d)
  (define (make-mobile left right)
    (cons left right))

  (define (make-branch length structure)
    (cons length structure))

  (define (left-branch mobile)
    (car mobile))
  (define (right-branch mobile)
    (cdr mobile))
  (define (branch-length branch)
    (car branch))
  (define (branch-structure branch)
    (cdr branch))

  (define (total-branch-weight branch)
    (if (number? (branch-structure branch))
        (branch-structure branch)
        (total-weight (branch-structure branch))))

  (define (total-weight mobile)
    (+ (total-branch-weight (left-branch mobile))
       (total-branch-weight (right-branch mobile))))

  (define (balanced? mobile)
    (define (torque branch)
      (* (branch-length branch) (total-branch-weight branch)))
    (let ((left-torque (torque (left-branch mobile)))
          (right-torque (torque (right-branch mobile))))
      ;(display "lt = ") (display left-torque) (newline)
      ;(display "rt = ") (display right-torque) (newline)
      (and (= left-torque right-torque)
           (or (number? (branch-structure (left-branch mobile))) (balanced? (branch-structure (left-branch mobile))))
           (or (number? (branch-structure (right-branch mobile))) (balanced? (branch-structure (right-branch mobile)))))))

  (display "exercise 2.29.d") (newline)

  (define x (make-mobile (make-branch 3 4)
                         (make-branch 3 (make-mobile (make-branch 4 5)
                                                     (make-branch 6 7)))))

  (define y (make-mobile (make-branch 1 3)
                         (make-branch 1 2)))

  (define un-balanced-mobile (make-mobile (make-branch 1 10)
                                          (make-branch 2 (make-mobile (make-branch 1 3)
                                                                      (make-branch 1 2)))))

  (define balanced-mobile (make-mobile (make-branch 1 12)
                                       (make-branch 2 (make-mobile (make-branch 1 4)
                                                                   (make-branch 2 2)))))

  (display "total weigth ") (display x) (display " = ") (display (total-weight x)) (newline)
  (display "total weight ") (display y) (display " = ") (display (total-weight y)) (newline)
  (display "balanced? ") (display x) (display ": ") (display (balanced? x)) (newline)
  (display "balanced? ") (display un-balanced-mobile) (display ": ") (display (balanced? un-balanced-mobile)) (newline)
  (display "balanced? ") (display balanced-mobile) (display ": ") (display (balanced? balanced-mobile)) (newline)
  (newline))
(exercise-2-29-d)

(define (exercise-2-30)
  (define (square x) (* x x))

  (define (square-tree-1 tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree-1 (car tree))
                      (square-tree-1 (cdr tree))))))

  (define (square-tree-2 tree)
    (map (lambda (tree)
           (if (pair? tree)
               (square-tree-2 tree)
               (square tree)))
         tree))

  (display "exercise 2.30") (newline)
  (define x (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7)))

  (display "test 1: ") (display (square-tree-1 x)) (newline)
  (display "test 2: ") (display (square-tree-2 x)) (newline)
  (newline))
(exercise-2-30)

(define (exercise-2-31)
  (define (square x) (* x x))

  (define (tree-map f tree)
    (map (lambda (tree)
           (if (pair? tree)
               (tree-map f tree)
               (f tree)))
         tree))

  (display "exercise 2.31") (newline)
  (define x (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7)))

  (display "(tree-map square x) = ") (display (tree-map square x)) (newline)
  (newline))
(exercise-2-31)

(define (exercise-2-32)
  (define (subsets s)
    ;(display "s = ") (display s) (newline)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
          ;(display "rest = ") (display rest) (display ", (car s) = ") (display (car s)) (newline)
          (append rest (map (lambda (x) (cons (car s) x)) rest)))))

  (display "exercise 2.32") (newline)
  (display (subsets (list 1 2 3))) (newline)
  (newline))
(exercise-2-32)

(define (exercise-2-33)
  (define (square x) (* x x))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

  (define (append seq1 seq2)
    (accumulate cons seq2 seq1))

  (define (length sequence)
    (accumulate (lambda (x y) (inc y)) 0 sequence))

  (display "exercise 2.33") (newline)
  (display "(map square (list 1 2 3)) = ") (display (map square (list 1 2 3))) (newline)
  (display "(append (list 1 2 3) (list 4 5 6)) = ") (display (append (list 1 2 3) (list 4 5 6))) (newline)
  (display "(length nil) = ") (display (length nil)) (newline)
  (display "(length (list 1 2 3)) = ") (display (length (list 1 2 3))) (newline)
  (newline))
(exercise-2-33)

(define (exercise-2-34)
  (define (square x) (* x x))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-term) (+ this-coeff (* x higher-term)))
                0
                coefficient-sequence))

  (display "exercise 2.34") (newline)
  (display "(horner-eval 2 (list 1 3 0 5 0 1)) = ") (display (horner-eval 2 (list 1 3 0 5 0 1))) (newline)
  (newline))
(exercise-2-34)

(define (exercise-2-35)
  (define (square x) (* x x))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (count-leaves tree)
    (accumulate + 0 (map (lambda (x) (cond ((null? x) 0)
                                           ((not (pair? x)) 1)
                                           (else (count-leaves x))))
                         tree)))

  (display "exercise 2.35") (newline)
  (define x (cons (list 1 2) (list 3 4)))
  (display "(count-leaves x) = ") (display (count-leaves x)) (newline)
  (display "(count-leaves (list x x)) = ") (display (count-leaves (list x x))) (newline)
  (newline))
(exercise-2-35)

(define (exercise-2-36)
  (define (square x) (* x x))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (accumulate-n op initial sequences)
    (if (null? (car sequences))
        nil
        (cons (accumulate op initial (map car sequences))
              (accumulate-n op initial (map cdr sequences)))))

  (display "exercise 2.36") (newline)
  (define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
  ;(display (map car x)) (newline)
  (display "(accumulate-n + 0 x) = ") (display (accumulate-n + 0 x)) (newline)
  (newline))
(exercise-2-36)

(define (exercise-2-37)
  (define (square x) (* x x))

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (accumulate-n op initial sequences)
    (if (null? (car sequences))
        nil
        (cons (accumulate op initial (map car sequences))
              (accumulate-n op initial (map cdr sequences)))))

  (define (dot-product v w)
    (accumulate + 0 (map * v w)))

  (define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m))

  (define (transpose m)
    (accumulate-n cons nil m))

  (define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
      (map (lambda (row)
             (map (lambda (col)
                    ;(display "row =") (display row) (display ", col = ") (display col) (newline)
                    (dot-product row col))
                  cols))
           m)))

  (display "exercise 2.37") (newline)
  (define v1 (list 1 2 3 4))
  (define v2 (list 4 5 6 6))
  (define v3 (list 6 7 8 9))
  (define m (list v1 v2 v3))
  (define n (list (list 1) (list 1) (list 1) (list 1)))
  (display "(dot-product v1 v2) = ") (display (dot-product v1 v2)) (newline)
  (display "(matrix-*-vector m v1) = ") (display (matrix-*-vector m v1)) (newline)
  (display "(transpose m) = ") (display (transpose m)) (newline)
  (display "(matrix-*-matrix m n)") (display (matrix-*-matrix m n)) (newline)
  (newline))
(exercise-2-37)

(define (exercise-2-38)
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (accumulate-n op initial sequences)
    (if (null? (car sequences))
        nil
        (cons (accumulate op initial (map car sequences))
              (accumulate-n op initial (map cdr sequences)))))

  (define (fold-right op initial sequence)
    (accumulate op initial sequence))

  (define (fold-left op initial sequence)
    (define (iter result sequence)
      (if (null? sequence)
          result
          (iter (op (car sequence) result) (cdr sequence))))
    (iter initial sequence))

  (define (reverse-right sequence)
    (define (append x y)
      (accumulate cons y x))
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

  (define (reverse-left sequence)
    (fold-left cons nil sequence))

  (display "exercise 2.38") (newline)
  (display "") (display (reverse-right (list 1 2 3 4))) (newline)
  (display "") (display (reverse-left (list 1 2 3 4))) (newline)
  (newline))
(exercise-2-38)
