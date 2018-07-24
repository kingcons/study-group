; Exercise 1.1:
; Below is a sequence of expressions. What is the result printed by the interpreter in
; response to each expression? Assume that the sequence is to be evaluated in the order
; in which it is presented.

; Answers delineated by ;

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ;
(define b (+ a 1)) ;
(+ a b (* a b)) ; 19
(= a b) ; #f

(if (and (> b a) (< b (* a b)))
    b
    a) ; 4?

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16

(+ 2 (if (> b a) b a)) ; 9?

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;8

; Exercise 1.2:
   (/ (+
        (+ 5 4)
          (- 2
            (- 3
              (+ 6
                (/ 4 5)))))
      (* 3
        (- 6 2)
        (- 2 7))
   )

; Exercise 1.3:
; Define a procedure that takes three numbers as arguments and returns the sum
; of the squares of the two larger numbers.

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (thingDoer x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        ((and (>= z x) (>= y x)) (sum-of-squares y z))
  )
)

; Exercise 1.4:
; Observe that our model of evaluation allows for combinations whose operators
; are compound expressions. Use this observation to describe the behavior of the
; following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Answer: this procedure takes and argument a and b. It then takes the absolute value of b by making b negative if it
; below zero (making it positive). it then adds a plus the new value of b and outputs the results.
