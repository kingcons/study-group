(defpackage :sicp-2.3
  (:use :cl))

(in-package :sicp-2.3)

;; Exercise 2.53

;; Common Lisp has ASSOC and GETF but lacks MEMQ.

(defun memq (item x)
  (cond ((null x) nil)
        ((eq item (car x)) x)
        (t (memq item (cdr x)))))

(list 'a 'b 'c)           ;; -> (a b c)
(list (list 'george))     ;; -> ((george))
(cdr '((x1 x2) (y1 y2)))  ;; -> ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;; -> (y1 y2)
(consp (car '(a short list)))           ;; -> nil
(memq 'red '((red shoes) (blue socks))) ;; -> nil
(memq 'red '(red shoes blue socks))     ;; -> (red shoes blue socks)

;; Exercise 2.54

(defun deep-equal? (xs ys)
  "Test the equality of arbitrary list structures.
  NOTE: This compares atoms with EQL so it won't work where EQL would return nil. E.g Strings."
  (cond ((and (atom xs)
              (atom ys))
         (eql xs ys))
        ((deep-equal? (car xs) (car ys))
         (deep-equal? (cdr xs) (cdr ys)))
        (t nil)))

;; Wound up looking at Eli Bendersky's solution for inspiration here.
;; Didn't like that his version was tree recursive. I also didn't like
;; that his solution didn't handle numbers.
;; So I wrote this which cheats(?) by using ATOM and EQL.
;; But I like the flow of:
;;   * If we're down to atoms punt to EQL.
;;   * If they aren't atoms, they're lists so ...
;;     * If the CARs are equal, keep "CDRing down" the list.
;;     * Otherwise, they're not equal and we're done.

;; Exercise 2.55

;; (car ''abracadabra) expands to -> (car (quote (quote abracadabra)))
;; Thus, the first element in the list is the symbol "quote".

;; Section 2.3.2

(defun variable? (x) (symbolp x))

(defun same-variable? (x y)
  (and (variable? x)
       (variable? y)
       (eq x y)))

(defun equal? (exp num)
  (and (numberp exp) (= exp num)))

(defun make-sum (x y)
  (cond ((equal? x 0) y)
        ((equal? y 0) x)
        ((and (numberp x) (numberp y))
         (+ x y))
        (t
         (list '+ x y))))

(defun addend (sum)
  (second sum))

(defun augend (sum)
  (third sum))

(defun make-product (x y)
  (cond ((or (equal? x 0)
             (equal? y 0))
         0)
        ((equal? x 1) y)
        ((equal? y 1) x)
        ((and (numberp x) (numberp y))
         (* x y))
        (t
         (list '* x y))))

(defun multiplier (product)
  (second product))

(defun multiplicand (product)
  (third product))

(defun sum? (sum)
  (and (consp sum) (eq (first sum) '+)))

(defun product? (product)
  (and (consp product) (eq (first product) '*)))

;; Exercise 2.56

(defun exponent? (exp)
  (and (consp exp) (eq (first exp) '**)))

(defun make-exponent (base exp)
  (cond ((equal? exp 0) 1)
        ((equal? exp 1) base)
        ((and (numberp base) (numberp exp))
         (expt base exp))
        (t
         (list '** base exp))))

(defun base (exp)
  (second exp))

(defun exponent (exp)
  (third exp))

(defun derive (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (derive (addend exp) var)
                   (derive (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (derive (multiplicand exp) var))
                   (make-product (derive (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponent? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponent (base exp)
                                                    (1- (exponent exp))))
                       (derive (base exp) var)))
        (t (error "Unknown expression type: ~A~%" exp))))

;; Exercise 2.57

;; The key insight here is that since the problem structure is already recursive,
;; you need only augment the constructors to handle arbitrary numbers of elements.
;; As with an earlier exercise, I'll cheat a bit by using APPLY. I really dislike this. :-/
;; Bendersky has a solution that is cleaner but ignores all the simplification rules.
;; I wonder if we can make the simplification rules play well with a recursive approach?

(defun make-sum (&rest args)
  (if (= (length args) 2)
      (cond ((equal? (first args) 0) (second args))
            ((equal? (second args) 0) (first args))
            ((and (numberp (first args))
                  (numberp (second args)))
             (+ (first args) (second args)))
            (t
             (cons '+ args)))
      (cons '+ (first args) (apply 'make-sum (rest args)))))

(defun make-product (&rest args)
  (if (= (length args) 2)
      (cond ((or (equal? (first args) 0)
                 (equal? (second args) 0))
             0)
            ((equal? (first args) 1) (second args))
            ((equal? (second args) 1) (first args))
            ((and (numberp (first args))
                  (numberp (second args)))
             (* (first args) (second args)))
            (t
             (cons '* args)))
      (cons '* (first args) (apply 'make-product (rest args)))))

;; Exercise 2.58 - SKIP for now

;; Exercise 2.59

;; Using the built-in ADJOIN and MEMBER functions in common lisp...


(defun total (items)
  (loop for i in items
        summing (parse-integer i) into total
        finally (return total)))

(defun union-set (set1 set2)
  (cond ((null set1) set2)
        ((member (car set1) set2)
         (union-set (cdr set1) set2))
        (t
         (union-set (cdr set1) (adjoin (car set1) set2)))))

;; Exercise 2.60

(defun includes-p (item set)
  (cond ((null set) nil)
        ((equal item (car set)) t)
        (t (includes-p item (cdr set)))))

(defun set-add (item set)
  (cons item set))

(defun set-union (set1 set2)
  (append set1 set2))

;; If we allow duplicates, the primary difference is that it doesn't break an invariant to
;; simplify additions and unions to just CONS the item on or APPEND the sets together.
;; For write-heavy applications this might be interesting, especially if you were confident
;; the data wouldn't often be repeated. Perhaps time series or something of that nature?
;; Anyway, inclusion checks and intersections will have the same time complexity but due to
;; potential repeated data you're likely to see worse real world times. It's just adds and
;; unions that get faster. As always, know your workload.

;; Exercise 2.61

(defun oset-add (item set)
  (cond ((null set) (cons item set))
        ((= (car set) item) set)
        ((< (car set) item) (cons (car set) (oset-add item (cdr set))))
        (t
         (cons item set))))

;; On average this should be faster than adjoin since we can just return the remainder
;; of the set once we've found our insertion point so we skip a bunch of pointer chasing.

;; Exercise 2.62

(defun oset-union (set1 set2)
  (cond ((null set1) set2)
        ((null set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (oset-union (cdr set1) set2)))
        ((= (car set1) (car set2))
         (cons (car set1) (oset-union (cdr set1) (cdr set2))))
        (t
         (cons (car set2) (oset-union set1 (cdr set2))))))

;; This is O(n) as we only process a given element from each set one time.

;; Subsection - Sets as Binary Trees

(defun make-tree (node left right)
  (list node left right))

(defun node (tree)
  (first tree))

(defun left-branch (tree)
  (second tree))

(defun right-branch (tree)
  (third tree))

(defun tree-includes-p (item tree)
  (cond ((null tree) nil)
        ((= item (node tree)) t)
        ((< item (node tree))
         (tree-includes-p item (left-branch tree)))
        (t
         (tree-includes-p item (right-branch tree)))))

(defun tree-adjoin (item tree)
  (cond ((null tree) (make-tree item nil nil))
        ((= item (node tree)) tree)
        ((< item (node tree))
         (make-tree (node tree)
                    (tree-adjoin item (left-branch tree))
                    (right-branch tree)))
        (t
         (make-tree (node tree)
                    (left-branch tree)
                    (tree-adjoin item (right-branch tree))))))

;; Exercise 2.63

(defvar *tree-a* '(7 (3 (1 nil nil) (5 nil nil)) (9 nil (11 nil nil))))
(defvar *tree-b* '(3 (1 nil nil) (7 (5 nil nil) (9 nil (11 nil nil)))))
(defvar *tree-c* '(5 (3 (1 nil nil) nil) (9 (7 nil nil) (11 nil nil))))

(defun tree->list-1 (tree)
  (if (null tree)
      nil
      (append (tree->list-1 (left-branch tree))
              (cons (node tree)
                    (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  (labels ((iter (tree result)
             (if (null tree)
                 result
                 (iter (left-branch tree)
                       (cons (node tree)
                             (iter (right-branch tree) result))))))
    (iter tree nil)))

;; 1. Yep, these both construct lists that maintain the ordering.
;; 2. These both seem to grow linearly with the trees as they visit each node
;; in constructing the list. That said, the list-2 version (the tail recursive one)
;; is on average about 20% faster on my machine based on:
;; (time (dotimes (i 100000) (mapcar #'tree->list-N (list *tree-a* *tree-b* *tree-c*))))
;; SBCL indicates that tree->list-1 conses about 40% more data. Perhaps due to append?

;; Exercise 2.64

