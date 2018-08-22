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
