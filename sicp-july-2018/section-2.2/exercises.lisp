(defpackage :sicp-2.2
  (:use :cl))

(in-package :sicp-2.2)

;; Exercise 2.17

(defun last-pair (items)
  (if (null (cdr items))
      items
      (last-pair (cdr items))))

;; Exercise 2.18

(defun my-reverse (items)
  (labels ((iter (sublist result)
             (if (null sublist)
                 result
                 (iter (cdr sublist)
                       (cons (car sublist) result)))))
    (iter items nil)))

;; Exercise 2.19 - SKIP for now

;; Exercise 2.20

(defun same-parity (&rest args)
  (let ((initial (mod (car args) 2)))
    (labels ((next-result (sublist result)
               (let ((new-parity (mod (car sublist) 2)))
                 (if (= new-parity initial)
                     (cons (car sublist) result)
                     result)))
             (iter (sublist result)
               (if (null sublist)
                   (my-reverse result)
                   (iter (cdr sublist)
                         (next-result sublist result)))))
      (iter args nil))))

;; Omg I hate it. Rewrite this shitty code. Maybe as non-tail recursive? :-/

;; Exercise 2.21

(defun my-map (proc items)
  (if (null items)
      nil
      (cons (funcall proc (car items))
            (my-map proc (cdr items)))))

(defun square-list-1 (items)
  (if (null items)
      nil
      (cons (expt (car items) 2)
            (square-list-1 (cdr items)))))

(defun square-list-2 (items)
  (my-map (lambda (item) (expt item 2)) items))

;; Exercise 2.22

;; Cons always attaches an item to the _front_ of the list, not the end.
;; Thus, if we iterate forwards over a list, each transformed item will
;; be preceded by any later items in the original list.

;; Interchanging the arguments to cons doesn't help since in a proper list,
;; the "tail" of the list should always be the second argument to CONS and
;; we supply it as the head. Box and pointer notation makes this particularly
;; clear but I won't transcribe that here. Suffice it to say that things are
;; in the right order but "inside out".

;; Exercise 2.23

(defun for-each (proc items)
  (if (null items)
      t
      (progn
        (funcall proc (car items))
        (for-each proc (cdr items)))))

;; Of course, it would be simpler to do this with DOLIST. Or MAPC.
;; In the absence of PROGN and DOLIST and MAPC I would create an
;; internal definition with LABELS to do the funcall and recurse.

(defun for-each-2 (proc items)
  (dolist (item items)
    (funcall proc item)))

(defun for-each-3 (proc items)
  (mapc proc items))

;; Exercise 2.24

;; Interpreter returns: (1 (2 (3 4)))
;; Box and pointer is (cheating a bit here):
;;   [ 1 ] -> [ 2 ] -> [ 3 4 ]
;; But note that this is not a proper list as it is not nil-terminated.
;; Tree:
;;    *
;;   / \
;;  1   *
;;     / \
;;    2   *
;;       / \
;;      3   4

;; Exercise 2.25

;; Assuming each list is defined as a variable x.
;; List 1: (cadr (car (cdr (cdr x))))
;; List 2: (caar x)
;; List 3: (cadr (cadr (cadr (cadr (cadr (cadr x))))))
;; Interesting how directly the 3rd answer corresponds with its example list.
;; One CADR for each level of nesting.

;; Exercise 2.26

;; (append x y) => (1 2 3 4 5 6)
;; (cons x y)   => ((1 2 3) 4 5 6)
;; (list x y)   => ((1 2 3) (4 5 6))

;; Exercise 2.27

(defun deep-reverse (tree)
  (labels ((process-item (item)
             (if (consp item)
                 (deep-reverse item)
                 item))
           (iter (sublist result)
             (if (null sublist)
                 result
                 (iter (cdr sublist)
                       (cons (process-item (car sublist)) result)))))
    (iter tree nil)))

;; Exercise 2.28

(defun fringe (items)
  (if (null items)
      nil
      ;; If it's a branch, we need to recurse to walk the branch
      ;; Otherwise, just cons the element and keep going.
      (if (consp (car items))
          (append (fringe (car items)) (fringe (cdr items)))
          (cons (car items) (fringe (cdr items))))))

(defun fringe (items)
  (labels ((iter (sublist result)
             (if (null sublist)
                 result
                 (let ((item (car sublist)))
                   (if (consp item)
                       (iter (cdr sublist) (append (iter item nil) result))
                       (iter (cdr sublist) (cons item result)))))))
    (my-reverse (iter items nil))))

;; Coming up with the iterative process version was a little tricky and
;; I wish that I had a better way to factor out the difference between:
;; (append (recurse (car items)) (recurse (cdr items)))
;; (cons (car items) (recurse (cdr items)))
;; I suppose we could always turn atoms into lists so we could append...

(defun leaves (tree)
  (cond ((null tree) nil)
        ((not (consp tree)) (list tree))
        (t (append (leaves (car tree))
                   (leaves (cdr tree))))))

(defun leaves-iter (tree)
  (labels ((iter (item result)
             (cond ((null item) result)
                   ((not (consp item)) (list item))
                   (t (let ((subtree (iter (car item) nil)))
                        (iter (cdr item) (append subtree result)))))))
    (my-reverse (iter tree nil))))

;; That's not bad for an iterative solution. But I wish there was a way to take out
;; the LABELS and still reverse the sequence only at the _end_.

;; Exercise 2.29

;;; Section 1

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (cadr mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cadr branch))

;;; Section 2 - SKIP for now

;; Exercise 2.30

(defun square-tree (tree)
  (mapcar (lambda (sub-tree)
            (if (consp sub-tree)
                (square-tree sub-tree)
                (expt sub-tree 2)))
          tree))

;; Exercise 2.31

(defun tree-map (proc tree)
  (mapcar (lambda (sub-tree)
            (if (consp sub-tree)
                (tree-map proc sub-tree)
                (funcall proc sub-tree)))
          tree))

(defun square-tree-map (tree)
  (tree-map (lambda (x) (* x x)) tree))

;; Exercise 2.32 - SKIP for now

;;; Section 2.2.3 - Sequences as Conventional Interfaces

(defun select (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence))
         (cons (car sequence)
               (select predicate (cdr sequence))))
        (t (select predicate (cdr sequence)))))

(defun accumulate (combiner initial sequence)
  (if (null sequence)
      initial
      (funcall combiner (car sequence)
               (accumulate combiner initial (cdr sequence)))))

;; Exercise 2.33

(defun my-map (proc sequence)
  (accumulate (lambda (x y) (cons (funcall proc x) y))
              nil sequence))

(defun my-append (xs ys)
  (accumulate #'cons ys xs))

(defun my-length (sequence)
  (flet ((my-count (x y)
           (declare (ignore x))
           (1+ y)))
    (accumulate #'my-count 0 sequence)))

;; Exercise 2.34 - SKIP for now

;; Exercise 2.35

(defun count-leaves (tree)
  (accumulate #'+ 0 (my-map (constantly 1) (leaves tree))))

;; This feels a bit like cheating but we have to traverse the tree somehow.
;; Reusing leaves to make it flat seems as reasonable as anything.
;; Also this is the first time I've had a clear need for CONSTANTLY. :)
;; Using only builtins:
;; (reduce #'+ (mapcar (constantly 1) (leaves tree)))

;; Exercise 2.36

(defun accumulate-n (op initial seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate   op initial (my-map #'car seqs))
            (accumulate-n op initial (my-map #'cdr seqs)))))

;; Exercise 2.37 - SKIP for now

(defun dot-product (vec1 vec2)
  (accumulate #'+ 0 (mapcar #'* vec1 vec2)))

;; Note the use of MAPCAR here: https://sarabander.github.io/sicp/html/2_002e2.xhtml#Footnote-78

;; Exercise 2.38

(defun fold-right (combiner initial sequence)
  (if (null sequence)
      initial
      (funcall combiner (car sequence)
               (fold-right combiner initial (cdr sequence)))))

(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
             (if (null rest)
                 result
                 (iter (funcall op result (car rest))
                       (cdr rest)))))
    (iter initial sequence)))

(macrolet ((fold-right (&rest args) `(accumulate ,@args)))
  (print (fold-right #'/     1  '(1 2 3)))  ;; => 3/2
  (print (fold-left  #'/     1  '(1 2 3)))  ;; => 1/6
  (print (fold-right #'list nil '(1 2 3)))  ;; => (1 (2 (3 NIL)))
  (print (fold-left  #'list nil '(1 2 3)))) ;; => (((NIL 1) 2) 3)

;; Exercise 2.39

(defun reverse-left (items)
  (fold-left (lambda (x y) (cons y x)) nil items))

;; I sort of know the path I took with reverse-right ...
;; but I don't feel I know the principles behind it. :-/
(defun reverse-right (items)
  (fold-right (lambda (x y) (append y (list x))) nil items))

;; Exercise 2.40

(defun smallest-divisor (n)
  (labels ((find-divisor (n test)
             (cond ((> (expt test 2) n) n)
                   ((zerop (rem n test)) test)
                   (t (find-divisor n (1+ test))))))
    (find-divisor n 2)))

(defun prime-p (n)
  (= n (smallest-divisor n)))

(defun prime-sum-p (pair)
  (prime-p (+ (first pair) (second pair))))

(defun make-pair-sum (pair)
  (append pair (list (+ (first pair) (second pair)))))

(defun enumerate (low high)
  (loop for i from low to high collecting i))

(defun flatmap (fn items)
  (accumulate #'append nil (my-map fn items)))

(defun unique-pairs (n)
  (flet ((construct-pairs-below (i)
           (my-map (lambda (j) (list i j))
                   (enumerate 1 (1- i)))))
    (flatmap #'construct-pairs-below (enumerate 1 n))))

(defun prime-sum-pairs (n)
  (my-map #'make-pair-sum
          (select #'prime-sum-p (unique-pairs n))))

;; Exercise 2.41

(defun unique-triples (n)
  (flet ((construct-triples-below (i)
           (flatmap (lambda (j)
                      (my-map (lambda (k)
                                (list i j k))
                              (enumerate 1 (1- j))))
                    (enumerate 1 (1- i)))))
    (remove nil (flatmap #'construct-triples-below (enumerate 1 n)))))

(defun triples-sum-to-n (n)
  (select (lambda (nums)
            (= n (reduce #'+ nums)))
          (unique-triples n)))
