(defpackage :sicp-1.1
  (:use :cl))

(in-package :sicp-1.1)

;; Exercise 1.2

;; in tree-form, ugh

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6
         2)
      (- 2
         7)))

;; arbitrarily

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3

;; Already provided in common lisp by (EXPT x 2)
;; (defun square (x)
;;   (* x x))

(defun sum-of-squares (x y z)
  (let ((args (list x y z)))
    (apply #'* (remove (min args) args))))

(defun sum-of-squares-alt (x y z)
  (cond ((and (< x y)
              (< x z))
         (+ (* y y) (* z z)))
        ((and (< y x)
              (< y z))
         (+ (* x x) (* z z)))
        (t
         (+ (* x x) (* y y)))))

;; Exercise 1.4

;; The procedure A-PLUS-ABS-B always return the sum of A and the absolute value of B.
;; It does this by subtracting B from A when B is negative and adding them otherwise.

;; Exercise 1.5

;; Normal Order languages have the interesting benefit of not needing special forms to
;; handle conditional semantics. In the TEST example, an Applicative Order Language
;; like Lisp, Javascript, Ruby, C, etc, will evaluate the arguments before they are
;; used. As a result it will go into an infinite loop while evaluating the P procedure.

;; In a Normal Order language, `(test 0 (p))` returns 0 since `(p)` would never be evaluated.
;; Nowadays, this is better known as "Lazy Evaluation" or "Call-By-Value" vs "Call-By-Need".

;; Section 1.1.7 - Newton's Method

(defun sqrt-iter (guess x)
  (if (good-enough-p guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough-p (guess x)
  (let ((threshold 0.001))
    (< (abs (- x (* guess guess))) threshold)))

(defun my-sqrt (x)
  (sqrt-iter 1.0 x))

;;; Super fancy common lisp version

(defun my-sqrt (x &optional (threshold 0.001))
  (labels ((average (x y)
             (/ (+ x y) 2))
           (improve (guess)
             (average guess (/ x guess)))
           (good-enough-p (guess)
             (let ((distance (abs (- x (expt guess 2)))))
               (< distance threshold)))
           (sqrt-iter (guess)
             (if (good-enough-p guess)
                 guess
                 (sqrt-iter (improve guess)))))
    (sqrt-iter 1.0)))


;; Exercise 1.6

;; If has to be a special form because otherwise it always evaluates both the then and else clauses.
;; The previous examples worked because the clauses were atoms, self-evaluating objects.

;; Exercise 1.7

(defun good-enough-p (guess x)
  (< (abs (- (improve guess x) guess))
     (* guess 0.001)))

;; Exercise 1.8

(defun curt-improve (guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))


(defun curt-good-enough-p (guess x)
  (< (abs (- (curt-improve guess x) guess))
     (* guess 0.001)))
