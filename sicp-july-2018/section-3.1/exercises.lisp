(defpackage :sicp-3.1
  (:use :cl))

(in-package :sicp-3.1)

;; Exercise 3.1
;; Two notes:
;; 1. There's no need for a begin (scheme) or progn (lisp) block since setf returns
;; the updated value by default in Common Lisp. I love this behavior, personally.
;; 2. It would be closer to the book's style to write "(setf foo (+ foo x))" but
;; this is exactly what the INCF macro is for so I'm using it. 🤘

(defun make-accum (sum)
  (lambda (x)
    (incf sum x)))

;; Exercise 3.2

(defun make-monitored (fn)
  (let ((count 0))
    (lambda (x)
      (cond ((eq x 'count) count)
            ((eq x 'reset) (setf count 0))
            (t
             (incf count)
             (funcall fn x))))))

(defun test-make-monitored ()
  (let ((monitor (make-monitored #'sqrt)))
    (funcall monitor 100)
    (assert (= 1 (funcall monitor 'count)))
    t))

;; Exercise 3.3 + 3.4

(defun make-account (balance secret &optional (attempts 7))
  (labels ((call-the-cops ()
             (error "They're trying to rob the bank!"))
           (withdraw (amount)
             (if (>= balance amount)
                 (decf balance amount)
                 "Insufficient funds"))
           (deposit (amount)
             (incf balance amount))
           (dispatch (password action)
             (cond ((not (eql password secret))
                    (if (zerop attempts)
                        (call-the-cops)
                        (progn
                          (decf attempts)
                          (lambda (x) :invalid-password))))
                   ((eql action 'withdraw) #'withdraw)
                   ((eql action 'deposit) #'deposit)
                   (t (error "Account Does Not Understand: ~A" action)))))
    #'dispatch))

(defun test-make-account ()
  (let ((account (make-account 100 'cookies)))
    (assert (eql (funcall (funcall account 'cookies 'withdraw) 40) 60))
    (assert (eql (funcall (funcall account 'courses 'withdraw) 40) :invalid-password))
    (dotimes (i 6)
      (funcall (funcall account 'courses 'withdraw) 40))
    (handler-case (funcall (funcall account 'courses 'withdraw) 100)
      (error () t)
      (:no-error (result) nil))))

;; Exercise 3.5
