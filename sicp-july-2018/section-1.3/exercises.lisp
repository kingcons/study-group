(defpackage :sicp-1.3
  (:use :cl))

(in-package :sicp-1.3)

;; Skipped Exercise 1.29

;; Exercise 1.30

(defun sum (a b term next &optional (result 0))
  (if (> a b)
      result
      (sum (funcall next a) b term next
           (+ result (funcall term a)))))

;; Alternately, we could write something closer to the book's style with labels.

(defun sum-labels (a b term next)
  (labels ((iter (a &optional (result 0))
             (if (> a b)
                 result
                 (iter (funcall next a)
                       (+ result (funcall term a))))))
    (iter a)))

;; Exercise 1.31

(defun product (a b term next &optional (result 1))
  (if (> a b)
      result
      (product (funcall next a) b term next
               (* result (funcall term a)))))

(defun factorial (n)
  (product 1 n #'identity #'1+))

(defun pi-approx (n)
  (flet ((pi-term (i)
           (float (/ (if (evenp i)
                         (+ i 2)
                         (+ i 1))
                     (if (evenp i)
                         (+ i 1)
                         (+ i 2))))))
    (* 4 (product 1 n #'pi-term #'1+))))

;; NOTE: pi-approx is interesting. If we compute to float at the end, bignum math makes things
;; super slow after a few thousand iterations. If we compute to float for every term, floating
;; point errors start to accumulate but things stay fast because we stay clear of bignums.

(defun product-rec (a b term next)
  (if (> a b)
      1
      (* (funcall term a)
         (product-rec (funcall next a) b term next))))

;; Exercise 1.32

(defun accumulate (a b term next combiner initial-value)
  (if (> a b)
      initial-value
      (accumulate (funcall next a) b term next combiner
                  (funcall combiner initial-value (funcall term a)))))

(defun sum-acc (a b term next)
  (accumulate a b term next #'+ 0))

(defun product-acc (a b term next)
  (accumulate a b term next #'* 1))

(defun accumulate-rec (a b term next combiner initial-value)
  (if (> a b)
      initial-value
      (funcall combiner (funcall term a)
               (accumulate-rec (funcall next a) b term next combiner initial-value))))

;; Exercise 1.33

(defun filtered-acc (a b term next filter combiner initial-value)
  (if (> a b)
      initial-value
      (filtered-acc (funcall next a) b term next filter combiner
                    (if (funcall filter a)
                        (funcall combiner initial-value (funcall term a))
                        initial-value))))

;; (filtered-acc 1 10 #'identity #'1+ #'evenp #'+ 0) => 30

;;;; Subproblem 1

;; In an ideal world, I'd export these definitions from sicp-1.2 and import them here.
;; But ain't nobody got time for a defsystem and all that. Self-contained sections make
;; more sense from a self-study point of view anyway. This ain't no shippin system! :)

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

(defun square (x) (* x x))

(defun sum-prime-squares (a b)
  (filtered-acc a b #'square #'1+ #'prime-number-p #'+ 0))

;;;; Subproblem 2

;; Of course both #'identity and #'gcd are part of the language standard. It's CL!

(defun product-relative-primes (a b)
  (flet ((relative-prime-p (i)
           (= (gcd i b) 1)))
    (filtered-acc a b #'identity #'1+ #'relative-prime-p #'* 1)))

;; Exercise 1.34

#|

Evaluating (f f) will first reduce to (f 2) and so the function will try to call itself with 2
as an argument. Then the interpreter will try to _call_ 2 and fail. This is a type error.
This is perhaps less likely to be hit in Common Lisp due to the distinct namespaces for functions
and symbols. It is certainly still possible to swap args around and get this error, of course.

|#

(defun fixed-point (fn initial-guess &optional (log nil) (tolerance 0.0001))
  (labels ((close-enough-p (x y)
             (< (abs (- x y)) tolerance))
           (try (guess)
             (let ((next (funcall fn guess)))
               (when log
                 (format t "Guess: ~A~%" guess))
               (if (close-enough-p guess next)
                   next
                   (try next)))))
    (try initial-guess)))

(defun fixed-sqrt (num)
  (flet ((average (x y)
           (/ (+ x y) 2)))
    (fixed-point (lambda (y) (average y (/ num y))) 1.0)))

;; Exercise 1.35

(defvar *golden-ratio* (fixed-point (lambda (x) (1+ (/ 1 x))) 1.0)) ;; => Yup, 1.618

;; Exercise 1.36

(defun find-x-log ()
 (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0 t))

(defun find-x-log-damped ()
  (flet ((average (x y)
           (/ (+ x y) 2)))
    (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0 t)))

;; FIND-X-LOG took 30 steps where FIND-X-LOG-DAMPED took 9 steps.
;; Interestingly, these are very close to the square root and cube root of 1000.

;; Exercise 1.37

;; So, the continued fraction can be computed using the fixed-point function
;; and 0.618 is the value of 1/golden-ratio. However, there is no need for any
;; of the numerator, denominator, or kth-term arguments in this example.

(defun cont-frac ()
  (fixed-point (lambda (x) (/ 1.0 (+ 1.0 x))) 2.0 t))

;; We could imagine extending the fixed-point function to "bail out" after a
;; given number of iterations, however...

(defun fixed-point-limit (fn initial-guess steps &optional (log nil) (tolerance 0.0001))
  (labels ((close-enough-p (x y)
             (< (abs (- x y)) tolerance))
           (try (guess step)
             (let ((next (funcall fn guess)))
               (when log
                 (format t "Guess ~A~%" guess))
               (if (or (close-enough-p guess next)
                       (zerop step))
                   next
                   (try next (1- step))))))
    (try initial-guess steps)))


;; (fixed-point-limit (lambda (x) (/ 1.0 (+ 1.0 x))) 2.0 10 t)
;; and here we see it takes about 10 steps to achieve a result with 4 digits of precision.
;; Now what about a dedicated cont-frac procedure?

(defun cont-frac (num denom k)
  (if (plusp k)
      (/ (funcall num k)
         (+ (funcall denom k)
            (cont-frac num denom (1- k))))
      0))

;; Yep, 11 steps to reach 0.6180. Now for an iterative process...

(defun cont-frac-iter (num denom k &optional (result 0))
  (if (plusp k)
      (cont-frac num denom (1- k)
                 (/ (funcall num k)
                    (+ (funcall denom k) result)))
      result))

;; Wow, that felt ... brain bendy. Summing result with denom felt wrong but worked.

;;;; Skipped 1.38, 1.39, 1.40

;; Exercise 1.41

(defun double-fn (fn)
  (lambda (x) (funcall fn (funcall fn x))))

;; It's interesting to see how different the separate namespaces make this look.
;; Not sure I'd say it's clearer or uglier really. Just very different.

(funcall (funcall (double-fn (double-fn #'double-fn)) #'1+) 5) ;; => 21

;; Exercise 1.42

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

(assert (= (funcall (compose #'square #'1+) 6) 49))

;; Exercise 1.43

(defun repeated (f n)
  (labels ((iter (i &optional result)
             (if (= i n)
                 result
                 (iter (1+ i) (compose f result)))))
    (iter 1 f)))

;; This is a program that just looks a little nicer recursively.

(defun repeated-rec (f n)
  (if (= n 1)
      f
      (compose f (repeated-rec f (1- n)))))

;; Section 1.3.4 code

(defun average-damp (fn)
  (lambda (x)
    (/ (+ x (funcall fn x)) 2)))

(defun damped-sqrt (x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(defun derive (fn &optional (threshold 0.00001))
  (lambda (x)
    (/ (- (funcall fn (+ x threshold))
          (funcall fn x))
       threshold)))

(defun newton-transform (fn)
  (lambda (x)
    (- x (/ (funcall fn x)
            (funcall (derive fn) x)))))

(defun newtons-method (fn guess)
  (fixed-point (newton-transform fn) guess))

(defun newton-sqrt (x)
  (newtons-method (lambda (y) (- (* y y) x)) 1))

(defun fixed-point-transform (fn transform guess)
  (fixed-point (funcall transform fn) guess))

(defun transform-sqrt (x)
  (fixed-point-transform (lambda (y) (/ x y)) #'average-damp 1.0))

(defun transform-newton-sqrt (x)
  (fixed-point-transform (lambda (y) (- (square y) x)) #'newton-transform 1.0))

;; Exercise 1.44

(defun smooth (fn &optional (threshold 0.00001))
  (lambda (x)
    (/ (+ (funcall fn (- x threshold))
          (funcall fn x)
          (funcall fn (+ x threshold)))
       3)))

(defun n-smoothed (fn n)
  (funcall (repeated #'smooth n) fn))

;; Exercise 1.45

;; (defun nth-root (n x)
;;   (let ((repeated-damp (repeated #'average-damp (sqrt n))))
;;     (fixed-point (funcall repeated-damp (lambda (y) (/ x (expt y (1- n)))))
;;                  1.0 t)))

(defun nth-root (n x)
  (flet ((find-root (y) (/ x (expt y (1- n)))))
    (let ((nth-damp (repeated #'average-damp (log n 2))))
      (fixed-point-transform #'find-root nth-damp 1.0))))

;; nth-root < 4, 1 damp
;; nth-root < 8, 2 damp
;; 3 dampings worked all the way up to 14th roots where I started hitting floating-point-overflow
;; I suspect at 16th roots it would've stopped converging but I'm not sure.
;; So that would mean you need (log n 2) dampings to keep converging but you're gonna hit
;; floating point overflow around 16th roots and need a new method anyhow AFAICT. :shrug:
;; Also, if I knew _where_ I should be forcing double float instead of single-float, maybe
;; I wouldn't overflow. But that's something for another day.

;; Exercise 1.46

;; So the weird thing about this at present is that, unlike SQRT or FIXED-POINT,
;; there isn't a second argument that seems obvious to pass to TEST-FN or IMPROVE-FN.
;; Like, in the case of SQRT and FIXED-POINT, our TEST-FN needs access to both a guess
;; and x/next. I'm going to assume the ITERATIVE-IMPROVE should take the _x_ as an arg,
;; _not_ the starting guess and see where that leads me. Then I can try defining sqrt
;; or fixed-point with each one and see where that leads me.

(defun my-iterative-improve (test-fn improve-fn)
  (labels ((iterate (guess x)
             (if (funcall test-fn guess)
                 guess
                 (iterate (funcall improve-fn guess) x))))
    (lambda (x &optional (initial 1.0)) (iterate initial x))))

(defun my-iter-sqrt (x)
  (flet ((good-enough-p (guess)
           (< (abs (- x (expt guess 2))) 0.000001))
         (improve (guess)
           (/ (+ guess (/ x guess)) 2)))
    (let ((improver (my-iterative-improve #'good-enough-p #'improve)))
      (funcall improver x))))

(defun my-iter-fixed (fn x &optional (initial 1.0))
  (flet ((good-enough-p (guess)
           (< (abs (- guess (funcall fn guess))) 0.000001)))
    (let ((improver (my-iterative-improve #'good-enough-p fn)))
      (funcall improver x initial))))

;; Okay, so I couldn't do the FIXED-POINT function without tweaking ITERATIVE-IMPROVE to
;; return a function that optionally allowed an initial parameter. I was testing with the
;; function that searched for x^x = 1000 which immediately divides by zero if the initial
;; guess is 1. That seems like further evidence that the book isn't in error. They want me
;; to take the GUESS as an argument and _not_ X. So let's try this the other way.
;; I suspect it will make the fixed point definition easy but the sqrt tough.

(defun iterative-improve (test-fn improve-fn)
  (labels ((iterate (guess)
             (if (funcall test-fn guess)
                 guess
                 (iterate (funcall improve-fn guess)))))
    #'iterate))

(defun improved-sqrt (x &optional (threshold 0.000001))
  (flet ((good-enough-p (guess)
           (< (abs (- x (expt guess 2))) threshold))
         (improve (guess)
           (/ (+ guess (/ x guess)) 2)))
    (let ((improver (iterative-improve #'good-enough-p #'improve)))
      (funcall improver 1.0))))

(defun improved-fixed (fn x &optional (threshold 0.000001))
  (flet ((good-enough-p (guess)
           (< (abs (- guess (funcall fn guess))) threshold)))
    (let ((improver (iterative-improve #'good-enough-p fn)))
      (funcall improver x))))

;; reimplementing find-x-log: (improved-fixed (lambda (x) (/ (log 1000) (log x))) 2.0) => 4.555

;; Interesting. So it seems like this actually _was_ simpler because in almost any function
;; where you'd want to _call_ ITERATIVE-IMPROVE, the x value to use would be present in the
;; lexical scope anyway. I'm guessing the purpose of this exercise was to get you to figure
;; out how to return a function that recursed as opposed to doing it immediately like
;; FIXED-POINT. I did it with labels and in Scheme, you'd do it with an internal DEFINE.
;; To do it with plain lambda, you'd have to reinvent the Y Combinator. Damn. Wonder if
;; they had any students do that?
