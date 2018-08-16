(defpackage :sicp-2.1
  (:use :cl))

(in-package :sicp-2.1)

;;; Rational Number Operations

(defun rations-+ (x y)
  (make-rational (+ (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

(defun rations-- (x y)
  (make-rational (- (* (numer x) (denom y))
                    (* (numer y) (denom x)))
                 (* (denom x) (denom y))))

(defun rations-* (x y)
  (make-rational (* (numer x) (numer y))
                 (* (denom x) (denom y))))

(defun rations-/ (x y)
  (make-rational (* (numer x) (denom y))
                 (* (numer y) (denom x))))

(defun rations-= (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; The Rational Number Interface

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun print-rational (x)
  (format t "~d/~d~%" (numer x) (denom x)))

;; Exercise 2.1

(defun make-rational (n d)
  (let* ((base (gcd n d))
         (num (/ n base))
         (den (/ d base)))
    (cond ((and (minusp num)
                (minusp den))
           (cons (abs num) (abs den)))
          ((minusp den)
           (cons (- num) (abs den)))
          (t
           (cons num den)))))

(defun make-rational (n d)
  (let ((g (if (< d 0)
               (- (gcd n d))
               (abs (gcd n d)))))
    (cond (/ n g)
          (/ d g))))

;; Exercise 2.2

(defun make-segment (start end)
  (cons start end))

(defun start-of (segment)
  (car segment))

(defun end-of (segment)
  (cdr segment))

(defun make-point (x y)
  (cons x y))

(defun x-of (point)
  (car point))

(defun y-of (point)
  (cdr point))

(defun print-point (point)
  (format t "(~d,~d)~%" (x-of point) (y-of point)))

(defun midpoint-of (segment)
  (flet ((average-coords (accessor)
           (/ (+ (funcall accessor (start-of segment))
                 (funcall accessor (end-of   segment)))
              2)))
    (make-point (average-coords #'x-of)
                (average-coords #'y-of))))

;; Example 2.3 - SKIP: for now

;; Example 2.4

;; I still love this one.

(defun my-cons (x y)
  (lambda (method) (funcall method x y)))

(defun my-car (obj)
  (obj (lambda (car cdr) car)))

(defun my-cdr (obj)
  (obj (lambda (car cdr) cdr)))

;; Example 2.5

(defun make-pair (a b)
  (* (expt 2 a)
     (expt 3 b)))

(labels ((find-exponent (number base &optional (count 0))
           (if (zerop (mod number (expt base (1+ count))))
               (find-exponent number base (1+ count))
               count)))
  (defun pair-first (pair)
    (find-exponent pair 2))
  (defun pair-last (pair)
    (find-exponent pair 3)))

;; So you could encapsulate FIND-EXPONENT this same way in scheme using internal defines
;; except then you'd be repeating yourself. Apparently, you can use the let syntax for
;; locally scoped recursive functions though: https://docs.racket-lang.org/reference/let.html

;; Example 2.6 - PASS

;; I don't care about church numerals right now so pass. :P

;; Example 2.7

(defun interval-+ (x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(defun interval-* (x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun interval-/ (x y)
  (let ((reciprocal (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y)))))
    (interval-* x reciprocal)))

(defun make-interval (ohms %-tolerance)
  (cons (- ohms (* ohms (* 0.01 %-tolerance)))
        (+ ohms (* ohms (* 0.01 %-tolerance)))))

(defun lower-bound (interval)
  (car interval))

(defun upper-bound (interval)
  (cdr interval))

;; Exercise 2.8

(defun interval-- (x y)
  (make-interval (- (lower-bound y)
                    (lower-bound x))
                 (- (upper-bound y)
                    (upper-bound x))))

;; I don't think I understand interval arithmetic which is really frustrating because I think
;; it's causing me to miss the bigger picture points about data abstraction. T_T

;; Exercise 2.9 - PASS

(defun interval-width (interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

;; Basically, I should be writing a proof here about the different behavior of interval
;; widths under multiplication and division vs addition and subtraction. I'm not about that life.
;; Sorry not sorry.

;; Exercise 2.10

(defun interval-/ (x y)
  ;; We only need to check y for spanning zero since we don't do any division on x.
  (unless (and (plusp (lower-bound y))
               (plusp (upper-bound y)))
    (error "It is unclear how to divide by an interval that crosses zero."))
  (let ((reciprocal (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y)))))
    (interval-* x reciprocal)))

;; Exercise 2.11 - SKIP: for now

;; Exercise 2.12

;; LOL, I basically already did this initially.
;; I think the constructor they were expecting me to write was:

(defun make-simple-interval (lower upper)
  (cons lower upper))

;; But why would I take the easy way out like that?
;; Doing it the "good" way isn't much harder?!? Here's the rest:

(defun interval-center (interval)
  (/ (+ (lower-bound interval)
        (upper-bound interval))
     2))

(defun interval-tolerance (interval)
  (/ (interval-width  interval)
     (interval-center interval)))

;; Exercise 2.13 - PASS

;; Exercise 2.14

(defun resistance-1 (x y)
  (interval-/ (interval-* x y)
              (interval-+ x y)))

(defun resistance-2 (x y)
  (let ((one (make-interval 1 0)))
    (interval-/ one
                (interval-+ (interval-/ one x)
                            (interval-/ one y)))))

(defvar *x* (make-interval 6.8 10))
(defvar *y* (make-interval 4.7 05))

;; Resistance-1 produces surprisingly incorrect results versus resistance-2.
;; Using the interval examples from earlier in the text I get quite close with resistance-2
;; but am off by a large margin using resistance-1.

