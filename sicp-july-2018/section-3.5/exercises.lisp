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

;;; Example Usage

(defun fibgen (a b)
  (make-lazy-list a (fibgen b (+ a b))))

(defvar *fibs* (fibgen 0 1))

(defun fib (n) (ref *fibs* n))
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

(defgeneric ref (list i)
  (:documentation "Access the Ith element of the LIST.")
  (:method ((list lazy-list) i)
    (if (zerop i)
        (head list)
        (ref (tail list) (1- i)))))

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
