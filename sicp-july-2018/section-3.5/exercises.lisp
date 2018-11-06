(defpackage :sicp-3.5
  (:use :cl))

(in-package :sicp-3.5)

;;; Lazy List Core Implementation

(defclass lazy-list ()
  ((head :initarg :head :accessor head)
   (tail :initarg :tail :accessor lazy-tail)))

(defmacro delay (&body body)
  "A simple call-by-name delay wrapping BODY in an argless lambda."
  `(lambda () ,@body))

(defun force (object)
  "Force the thunked/delayed OBJECT."
  (funcall object))

(defgeneric tail (list)
  (:documentation "Force the delayed tail of a LIST.")
  (:method ((list lazy-list))
    (force (lazy-tail list))))

(defmacro make-lazy-list (head tail)
  `(make-instance 'lazy-list :head ,head :tail (delay ,tail)))

(defgeneric lref (list i)
  (:documentation "Access the Ith element of the LIST.")
  (:method ((list lazy-list) i)
    (if (zerop i)
        (head list)
        (lref (tail list) (1- i)))))

;;; Example Usage

(defun fibgen (a b)
  (make-lazy-list a (fibgen b (+ a b))))

(defvar *fibs* (fibgen 0 1))

(defun fib (n) (lref *fibs* n))
(defun show-fib (n) (format t "The ~dth fibonacci number is ~d~%" n (fib n)))

;;; Memoized Lazy List Variant

(defun memoize (fn)
  (let ((executed-p nil)
        (result nil))
    (lambda ()
      (unless executed-p
        (setf result (funcall fn)
              executed-p t))
      result)))

(defmacro delay-memo (&body body)
  "A memoizing call-by-need delay wrapping BODY in a lambda."
  `(memoize (lambda () ,@body)))

(defmacro make-lazy-memo (head tail)
  `(make-instance 'lazy-list :head ,head :tail (delay-memo ,tail)))

;;; Lazy List operations

(defgeneric lmap (fn list)
  (:documentation "Map over the Lazy LIST with FN.")
  (:method (fn list)
    (if (null list)
        nil
        (make-lazy-list
         (funcall fn (head list))
         (lmap fn (tail list))))))

(defun liota (low high)
  "Generate a lazy list from LOW to HIGH."
  (if (> low high)
      nil
      (make-lazy-list low (liota (1+ low) high))))

(defgeneric each (fn list)
  (:documentation "Iterate over LIST executing FN.")
  (:method (fn (list lazy-list))
    (cond ((null list)
           :done)
          (t
           (funcall fn (head list))
           (each fn (tail list))))))

(defgeneric filter (fn list)
  (:documentation "Filter the lazy LIST through FN.")
  (:method (fn (list lazy-list))
    (cond ((null list)
           nil)
          ((funcall fn (head list))
           (make-lazy-list (head list)
                           (filter fn (tail list))))
          (t
           (filter fn (tail list))))))

(defgeneric display (list)
  (:documentation "Display the entire stream.")
  (:method (list)
    (each (lambda (x) (format t "~%~A" x)) list)))

;;; Exercises

;; Exercise 3.50

(defun stream-map (fn &rest streams)
  (if (null (car streams))
      nil
      (make-lazy-memo
       (apply fn (mapcar 'head streams))
       (apply 'stream-map (cons fn (mapcar 'tail streams))))))

;; Exercise 3.51

(defvar *nums* (liota 1 10))

(defun show (x)
  (format t "~A~%" x)
  x)

;; Only 0 is printed when the stream-map is constructed.
;; 1-5 are printed on referencing 5 and 1-7 are printed on referencing 7.
;; NOTE: I did not use the lazy-memo version for this.
;; Presumably if I had only 6 and 7 would've printed for (lref 7).
;; Footnote 187 that goes with this exercise is quite thought provoking.

;; Exercise 3.52

(defvar *sum* 0)

(defun accum! (x)
  (incf *sum* x))

(defvar *seq* (stream-map #'accum! (liota 1 20)))
(defvar *evens* (filter #'evenp *seq*))
(defvar *mod-5* (filter (lambda (x) (zerop (rem x 5))) *seq*))
