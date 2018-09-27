(defpackage :sicp-2.4
  (:use :cl))

(in-package :sicp-2.4)

;; High-level Interface

(defun add-complex (c1 c2)
  (make-rect-complex :real (+ (real-part c1) (real-part c2))
                     :imaginary (+ (imaginary-part c1) (imaginary-part c2))))

(defun sub-complex (c1 c2)
  (make-rect-complex :real (- (real-part c1) (real-part c2))
                     :imaginary (- (imaginary-part c1) (imaginary-part c2))))

(defun mul-complex (c1 c2)
  (make-polar-complex :magnitude (* (magnitude c1) (magnitude c2))
                      :angle (+ (angle c1) (angle c2))))

(defun div-complex (c1 c2)
  (make-polar-complex :magnitude (/ (magnitude c1) (magnitude c2))
                      :angle (- (angle c1) (angle c2))))

;; Type Tagging Machinery

(defun attach-tag (tag data)
  (cons tag data))

(defun type-tag (object)
  (if (consp object)
      (car object)
      (error "NO TYPE-TAG: ~A" object)))

(defun instance (object)
  (if (consp object)
      (cdr object)
      (error "NO INSTANCE: ~A" object)))

(defun rectangular-p (complex)
  (eq (type-tag complex) 'rectangular))

(defun polar-p (complex)
  (eq (type-tag complex) 'polar))

;; Rectangular Methods

(defun real-part-rectangular (complex)
  (car complex))

(defun imaginary-part-rectangular (complex)
  (cdr complex))

(defun magnitude-rectangular (complex)
  (sqrt (+ (expt (real-part-rectangular complex) 2)
           (expt (imaginary-part-rectangular complex) 2))))

(defun angle-rectangular (complex)
  (atan (imaginary-part-rectangular complex)
        (real-part-rectangular complex)))

(defun make-rectangular-complex (&key real imaginary magnitude angle)
  (cond ((and real imaginary)
         (attach-tag 'rectangular (cons real imaginary)))
        ((and magnitude angle)
         (let ((real-part (* magnitude (cos angle)))
               (imaginary-part (* magnitude (sin angle))))
           (attach-tag 'rectangular (cons real-part imaginary-part))))
        (t (error "INVALID INPUT: Must have real and imaginary _or_ magnitude and angle."))))

;; Polar Methods

(defun real-part-polar (complex)
  (* (magnitude-polar complex) (cos (angle-polar complex))))

(defun imaginary-part-polar (complex)
  (* (magnitude-polar complex) (sin (angle-polar complex))))

(defun magnitude-polar (complex)
  (car complex))

(defun angle-polar (complex)
  (cdr complex))

(defun make-polar-complex (&key real imaginary magnitude angle)
  (cond ((and real imaginary)
         (let ((real-part (sqrt (+ (expt real 2) (expt imaginary 2))))
               (imaginary-part (atan imaginary real)))
           (attach-tag 'polar (cons real-part imaginary-part))))
        ((and magnitude angle)
         (attach-tag 'polar (cons magnitude angle)))
        (t (error "INVALID INPUT: Must have real and imaginary _or_ magnitude and angle."))))

;; Generic Functions

(defun dispatch (complex rect-fn polr-fn)
  (cond ((rectangular-p complex) (funcall rect-fn (instance complex)))
        ((polar-p complex)       (funcall polr-fn (instance complex)))
        (t (error "TYPE ERROR: Not a known complex number ~A" (type-tag complex)))))

(defun real-part (complex)
  (dispatch complex #'real-part-rectangular #'real-part-polar))

(defun imaginary-part (complex)
  (dispatch complex #'imaginary-part-rectangular #'imaginary-part-polar))

(defun magnitude (complex)
  (dispatch complex #'magnitude-rectangular #'magnitude-polar))

(defun angle (complex)
  (dispatch complex #'angle-rectangular #'angle-polar))

;; Data-Directed Style

;; I could rewrite the above code to be in SICP-style "packages"
;; by using LABELS to define a bunch of local functions and then
;; using (setf (get symbol 'indicator) value) to place those functions
;; into the symbols property list. GET and APPLY work as shown in SICP.

;; Exercise 2.73

(defun derive (exp var)
  (flet ((operator (exp) (car exp))
         (operands (exp) (cdr exp)))
    (cond ((numberp exp) 0)
          ((and (symbolp exp)
                (symbolp var))
           (if (eq exp var) 1 0))
          (t
           (funcall (get 'derive (operator exp))
                    (operands exp) var)))))

;; 1. This is a fairly straightforward refactoring to store functions in the "derive" symbol
;; plist for handling the various kinds of expressions we'd want to derive. This won't work
;; for numbers and vars since they're atoms and the data directed dispatch is currently based
;; on lists.

;; 2.

(flet (()))

;; Exercise 2.74

;; Exercise 2.75

;; Exercise 2.76

;; This is the expression problem! https://en.wikipedia.org/wiki/Expression_problem
;; Ignoring issues of multiple dispatch...
;; When adding a new type:
;;   * Explicit dispatch -> every generic operation handling that type must be modified
;;     * this is basically CL typecase
;;   * Data-directed style -> a package for the type must be installed/table updated
;;   * Message-passing style -> only "receiving" operations must be modified
;; When adding a new operation:
;;   * Explicit dispatch -> Only the new operation needs to be written
;;   * Data-directed style -> separate implementations for all supported types, installer
;;   * Message-passing style -> Add a dispatch clause to all supported types
