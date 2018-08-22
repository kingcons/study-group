(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define squares
  (list 1 4 9 16 25))

(list-ref squares 3)
(list-ref '(4 9 16 25)
            2)
(list-ref '(9 16 25)
            1)
(list-ref '(16 25)
            0)
16

; *****************************

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds
  (list 1 3 5 7))

(length odds)
(+ 1 (length '(3 5 7)))
(+ 1 (+ 1 (length '(5 7))))
(+ 1 (+ 1 (+ 1 (length '(7)))))
(+ 1 (+ 1 (+ 1 (+ 1 (length null)))))
(+ 1 (+ 1 (+ 1 (+ 1 0))))
(+ 1 (+ 1 (+ 1 1)))
(+ 1 (+ 1 2))
(+ 1 3)
4

; *******************************















; Exercise 2.21: The procedure square-list takes a list of numbers
; as argument and returns a list of the squares of those numbers.

(square-list (list 1 2 3 4))
(1 4 9 16)

; Here are two different definitions of square-list. Complete both
; of them by filling in the missing expressions:

(define (square-list items)
  (if (null? items)
      null
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map ⟨??⟩ ⟨??⟩))
