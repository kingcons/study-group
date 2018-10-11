(defpackage :sicp-3.3
  (:use :cl))

(in-package :sicp-3.3)

;; Exercise 3.12

(defun append! (x y)
  (setf (cdr (last x)) y))

;; Since append is non-destructive, the cdr of X is (B).
;; Since append! is destructive, the cdr of X is (C D).
;; In the latter case, the reference to B becomes garbage.

;; Exercise 3.13

;; Make-cycle simply sets the final pointer in the list to the head of the list.
;; AKA circular list or ring buffer: https://en.wikipedia.org/wiki/Circular_buffer
;; Guile Scheme seems to detect cyclic structures so printing them is a non-issue.
;; In Common Lisp, one must be careful to bind *print-circle* appropriately.
;; Or set it globally if they'll be commonly worked with in the REPL.
;; There is a literal syntax for circular lists in CL: #1=(1 2 3 . #1#)

(defun make-cycle (items)
  (setf (cdr (last items)) items))

(let ((foo '(1 2 3))
      (*print-circle* t))
  (make-cycle foo)
  (write foo)
  :done)

;; Seeking the last pair of a ring buffer or circular list will loop infinitely.
;; You could work around this by tracking previously seen pointers/references.
;; I.e. While looking for the last pair of X, check if the CDR is EQ to X.
;; Common Lisp's EQ is basically an object identity/pointer equality check.
;; Presumably Lisp does this or something smarter when *PRINT-CIRCLE* is t.

;; Exercise 3.14

(defun mystery (items)
  (labels ((iter (x y)
             (if (null x)
                 y
                 (let ((temp (cdr x)))
                   (format t "x: ~A, y: ~A, temp: ~A~%" x y temp)
                   (setf (cdr x) y)
                   (format t "New 'x': ~A, new 'y': ~A~%" temp x)
                   (iter temp x)))))
    (iter items nil)))

;; In my opinion, there are 2 ways to view this: Box & Pointer or State Variables.
;; State variables we see a clear pattern: temp is used to CDR down the list and
;; y has the first item from the list appended to it each step, reversing x.
;; The box and pointer view makes the pointer/reference swapping more clear.
;; Temp holds a pointer to the list's tail. Each step, we set the list's tail to y,
;; effectively putting the front item in X at the head of Y. Then we pass along
;; the old "temp" to be our X next time and the cdr-modified "x" to be our new Y.
;; So: w -> '(d c b a) and v -> '(1) since we start by setting CDR of items to nil.

;; Exercise 3.15

;; Nah.

;; Exercise 3.16

(defun count-pairs (items)
  (if (not (consp items))
      0
      (+ (count-pairs (car items))
         (count-pairs (cdr items))
         1)))

(let* ((x (cons 1 2))
       (y (cons 'a 'b))
       (z (cons x y)))
  (assert (= (count-pairs z) 3))
  (setf (cdr x) y)
  (assert (= (count-pairs z) 4)))

;; Any circular list will cause this procedure to never return at all.
;; The 7 element version can't be done as above but works fine with a plain list.
;; The nested cons structure keeps you from getting multiple pointers to 3 pairs.

;; Exercise 3.17

(defun real-count-pairs (items)
  ;; TODO
  )

;; Exercise 3.18

(let ((*print-circle* t))
  (defun circular? (items &optional seen)
    (cond ((null (cdr items))
           nil)
          ((find (cdr items) seen)
           t)
          (t
           (circular? (cdr items) (cons items seen))))))

;; This was tricky as using push or append got the infinite lists tangled up.
;; Sticking to just consing the current pointer on the front and then
;; checking the CDR before recursing seems like "the right thing".

;; Exercise 3.19

;; This is a classic interview problem (that I have never faced).
;; Maintain two pointers and move one forward by a single link and one by two.
;; I.e. Step one by CDR and one by CDDR, if there is a cycle the second pointer
;; will eventually "lap" the first one and they'll be equal.
;; So your cases are:
;;  * NULL -> no cycle
;;  * CDR == CDDR -> cycle
;;  * otherwise -> recursive case

;; Exercise 3.20

