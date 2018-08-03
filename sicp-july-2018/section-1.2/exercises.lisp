(defpackage :sicp-1.2
  (:use :cl))

(in-package :sicp-1.2)

;; Exercise 1.9

(defun inc (x) (1+ x))
(defun dec (x) (1- x))

(defun add (a b)
  (if (zerop a)
      b
      (inc (add (dec a) b))))

(defun add-iter (a b)
  (if (zerop a)
      b
      (add-iter (dec a) (inc b))))

;;; We can see from the fact that ADD-ITER is a tail call which only updates A and B.
;;; This assures me that it is a recursive procedure but an iterative process.

;;; ADD, however, is a recursive procedure and a recursive process as it builds a chain of
;;; deferred (inc (inc (inc operations. This can be clearly seen with Lisp's glorious TRACE.
;;; But I won't copy and paste it here cause it kinda _big_.

;; Exercise 1.10

;; ONLY TRACE ACK IF YOU ARE BRAVE. Or do it in the terminal at least. Use small numbers.

(defun ack (x y)
  (cond ((zerop y) 0)
        ((zerop x) (* 2 y))
        ((= y 1)   2)
        (t (ack (1- x)
                (ack x (1- y))))))

;; (ack 1 10) -> 1024  (2 ^ 10)
;; (ack 2 4)  -> 65536 (2 ^ 16)
;; (ack 3 3)  -> 65536 (2 ^ 16)

;; (f n) computes 2n
;; (g n) computes 2^n
;; (h n) computes 2^(2^n)

;; You'd think there's a way to algebraically work this out... but maybe not.
;; Maybe that's why Ackermann's function is famous.

;; Exercise 1.11

;; Recursive Process

(defun f (n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; I'm skipping iterative version cause I don't want to shove all the parts into state variables.
;; Maybe if they had names they would be more endeared to me. Also, I did this once in 2008. :P

;; Exercise 1.12

;; Discussion point: Do I actually need to test (= row 1)? Why or why not?
(defun pascal (row col)
  (if (or (= row 1) (= col 1) (= row col))
      1
      (+ (pascal (- row 1)
                 (- col 1))
         (pascal (- row 1)
                 col))))

;; Exercise 1.13

;;; NOPE NOPE NOPE

;; Exercise 1.14

;;; Maybe in discussion. Also NOPE

;; Exercise 1.15

;; Ugh, fine. Substitution model...
#|
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
|#

;; This algorithm for sines grows in log(angle) time.
;; The amount of work to be done in a logarithmic algorithm is divided each step.

;; Exercise 1.16

(defun square (x)
  (* x x))

(defun fast-expt-recursive (b n)
  (cond ((zerop n) 1)
        ((evenp n) (square (fast-expt-recursive b (/ n 2))))
        (t (* b (fast-expt-recursive b (1- n))))))

(defun fast-expt (base exp &optional (result 1))
  (cond ((zerop exp)
         result)
        ((evenp exp)
         (fast-expt (square base) (/ exp 2) result))
        (t
         (fast-expt base (1- exp) (* base result)))))

;; Exercise 1.17

(defun mul (a b)
  (if (zerop b)
      0
      (+ a (mul a (1- b)))))

(defun my-double (x)
  (+ x x))

(defun fast-mul-recursive (a b)
  (cond ((zerop b)
         0)
        ((evenp b)
         (my-double (fast-mul-recursive a (/ b 2))))
        (t
         (+ a (fast-mul-recursive a (1- b))))))

;; Exercise 1.18

(defun fast-mul (a b &optional (product 0))
  (cond ((zerop b)
         product)
        ((evenp b)
         (fast-mul (+ a a) (/ b 2) product))
        (t
         (fast-mul a (1- b) (+ a product)))))

;; The pattern between fast-expt and fast-mul is identical.
;; It's interesting as the invariants are similar.
;; fast-expt: ab^n are equal in every state, fast-mul: ab+n equal in every state

;; That said, I had to fight hard against the urge to try and compute the final result in the
;; "zero?" case. Which I suppose would be fine if I just passed along the first and last arg.
;; tl;dr: Zero and even cases felt "obvious", odd/final case felt hard to see.
;; I guess some part of me wants a "exp == 1" case, where we do the final "combine" operation.

;; Exercise 1.19

;; Nah, I'm kinda over "move stack operations into args that maintain invariant" for today.

;; Exercise 1.20

;; Also Nope!

;;;; Assorted Section 1.2.6 bits and bobs

(defun smallest-divisor (n)
    (find-divisor n 2))

(defun divides-p (a b)
  (zerop (rem b a)))

(defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides-p test-divisor n)
           test-divisor)
          (t (find-divisor
              n
              (+ test-divisor 1)))))


(defun prime-number-p (n)
  (= n (smallest-divisor n)))

(defun expmod (base exp m)
  (cond ((zerop exp)
         1)
        ((evenp exp)
         (rem (square (expmod base (/ exp 2) m)) m))
        (t
         (rem (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (flet ((maybe-prime-p (a)
           (= (expmod a n n) a)))
    (maybe-prime-p (1+ (random (1- n))))))

(defun fast-prime-p (number trials)
  (cond ((zerop trials) t)
        ((fermat-test number) (fast-prime-p n (1- times)))
        (t nil)))

;; Exercise 1.21

(smallest-divisor 199)   ; => 199
(smallest-divisor 1999)  ; => 1999
(smallest-divisor 19999) ; => 7

;; Exercise 1.22

(defun timed-prime-test (n)
  (start-prime-test n (get-internal-real-time)))

(defun start-prime-test (n started-at)
  (when (prime-number-p n)
    (format t " *** ~d found, ~a milliseconds~%"
            n (- (get-internal-real-time) started-at))))

(defun search-for-primes (start end)
  (let ((ran-at (get-internal-real-time)))
    (format t "~& ** PRIME SEARCH BEGINNING ** ~%")
    (loop for num = start then (+ num 2)
          while (< num end)
          do (timed-prime-test num))
    (format t "~& ** SEARCH FINISHED (~d ms) ** ~%"
            (- (get-internal-real-time) ran-at))))

;; 3 smallest > than 1000    - 1009, 1013, 1019
;; 3 smallest > than 10000   - 10007, 10009, 10037
;; 3 smallest > than 100000  - 100003, 100019, 100043
;; 3 smallest > than 1000000 - 1000003, 1000033, 1000037

;; On modern hardware, actually observing these steps is gonna take some doing.
;; Let's start in the 10 millions.

;; 3 smallest > than 10M - 10000019, 10000079, 10000103 (from 10M - 10,000,500 5ms)
;; 3 smallest > than 100M - 100000007, 100000037, 100000039 (from 100M - 100,000,500 12ms)
;; 1,000,000,000 to 1,000,000,500 took 30ms
;; 10B -> 10b500 took 61ms

;; Each digit should've roughly tripled the search time and that _about_ matches up,
;; though our scaling factor is closer to 2.5 than the suggested sqrt(10) ~ 3.15.

;; Exercise 1.23

