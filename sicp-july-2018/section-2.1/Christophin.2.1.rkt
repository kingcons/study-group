; Exercise 2.1: Define a better version of make-rat that handles both positive and negative arguments.
; Make-rat should normalize the sign so that if the rational number is positive, both the numerator
; and denominator are positive, and if the rational number is negative, only the numerator is negative.

; Original
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

(define (gcd2 a b)
  (if (= b 0)
      a
      (gcd2 b (remainder a b))))

(define (Make-rat n d)
    (let ((g ((cond ((and (< n 0) (< d 0)) -)
                ((and (> n 0) (< d 0)) -)
               (else +))
               (gcd n d))))
        (cons (/ n g)
        (/ d g))))

(define (Make-rat n d)
    (let ((g ((if (< d 0) - +) (abs (gcd2 n d)))))
        (cons (/ n g)
              (/ d g))))

 (define (bi-dir-fib n)
    (cond ((or (= n 0) (= n 1)) n)
          ((> n 1) (+ (bi-dir-fib (- n 2)) (bi-dir-fib (- n 1))))
          (else (* (expt (- 1) (+ (abs n) 1)) (bi-dir-fib (abs n))))))
