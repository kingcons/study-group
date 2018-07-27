; Exercise 1.1:
; Below is a sequence of expressions. What is the result printed by the interpreter in
; response to each expression? Assume that the sequence is to be evaluated in the order
; in which it is presented.

; Answers for this problem delineated by ;

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a
(define b (+ a 1)) ; b
(+ a b (* a b)) ; 19
(= a b) ; #f

(if (and (> b a) (< b (* a b)))
    b
    a) ; 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16

(+ 2 (if (> b a) b a)) ; 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 8

; Exercise 1.2:
   (/ (+
        (+ 5 4)
          (- 2
            (- 3
              (+ 6
                (/ 4 5)))))
      (* 3
        (- 6 2)
        (- 2 7)))

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

; ***ANSWER***: this procedure takes and argument a and b. It then takes the absolute
; value of b by making b negative if it below zero (making it positive).
; it then adds a plus the new value of b and outputs the results.

; Exercise 1.5: Ben Bitdiddle has invented a test to determine whether the interpreter
; he is faced with is using applicative-order evaluation or normal-order evaluation.
; He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
; Then he evaluates the expression

(test 0 (p))
; What behavior will Ben observe with an interpreter that uses applicative-order
; evaluation? What behavior will he observe with an interpreter that uses
; normal-order evaluation? Explain your answer. (Assume that the evaluation rule
; for the special form if is the same whether the interpreter is using normal or
; applicative order: The predicate expression is evaluated first, and the result
; determines whether to evaluate the consequent or the alternative expression.)

; ***ANSWER***: If the interpreter runs this expression using applicative-order evaluation,
; the expression (p) will be called, causing an infinite loop. This kills the Bitdiddle.
; The reason this breaks is because applicative-order evaluation reduces everything to
; atoms before evaluation. If the interpreter runs the expression using normal-order
; evaluation, the predicate is evaluated first. In the example above, we see x = 0. so
; (= 0 0) evaluates to true. the interpreter will then move directly to returning whats
; on the true side, in this case 0. (p) is never examined.
; What's cool about this to me is it that it shows how unique if statements are.
; most coding languages use applicative-order evaluation, except for their if statements.
; by using normal order evaluation, if statements allows use to skip over the evaluation of
; we don't want to trigger.



; Exercise 1.7: The good-enough? test used in computing square roots will not be
; very effective for finding the square roots of very small numbers. Also, in real
; computers, arithmetic operations are almost always performed with limited precision.
; This makes our test inadequate for very large numbers. Explain these statements,
; with examples showing how the test fails for small and large numbers.
; An alternative strategy for implementing good-enough? is to watch how guess
; changes from one iteration to the next and to stop when the change is a very
; small fraction of the guess. Design a square-root procedure that uses this kind
; of end test. Does this work better for small and large numbers?
