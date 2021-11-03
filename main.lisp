;; Look up documentation for the symbol at
;; point in the Common Lisp Hyperspec
;; C-c C-d h

;; Problem 01.
(defun my-last (xs)
  (if (= (length xs) 1)
      (first xs)
      (my-last (rest xs))))


;; Problem 02.
(defun my-but-last (xs)
  (if (= (length xs) 2)
      (list (first xs) (second xs))
      (my-but-last (rest xs))))


;; Problem 03.
(defun element-at (xs idx)
  (if (= idx 1)
      (first xs)
      (element-at (rest xs) (1- idx))))


;; Problem 04.
(defun my-length (xs)
  (if (null xs)
      0
      (1+ (my-length (rest xs)))))


;; Problem 05.
(defun my-reverse (xs)
  (reverse-aux xs ()))

(defun reverse-aux (xs rst)
  (if (null xs)
      rst
      (reverse-aux (rest xs) (cons (first xs) rst))))


;; Problem 06.
(defun palindrome-p (xs)
  (equal xs (reverse xs)))


;; Problem 07.
(defun flatten (xs)
  (if (not (null xs))
      (if (listp (first xs))
	  (append (flatten (first xs)) (flatten (rest xs)))
	  (append (cons (first xs) nil) (flatten (rest xs))))))


;; Problem 08.
(defun last-element (xs)
  "Get last element from list as an element, not as list"
  (if (not (null xs))
      (if (listp xs)
	  (first (last xs))
	  xs)))

(defun compress (xs)
  (if (not (null xs))
      (cons
       (first xs) ;; todo
       (rest xs))))

;; Example:
;; (compress '(a a a a b c c a a d e e e e)) => (A B C A D E)
