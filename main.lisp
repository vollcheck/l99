;; Look up documentation for the symbol at
;; point in the Common Lisp Hyperspec `C-c C-d h`

;; Problem 01.
(defun my-last (xs)
  (if (= (length xs) 1)
      (first xs)
      (my-last (rest xs))))


;; Problem 02.
(defun my-butlast (xs)
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
(defun my-reverse (xs &optional (acc ())) ; accumulator design
  (if (null xs)
      acc
      (my-reverse (rest xs) (cons acc (first xs)))))


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
(defun compress (xs)
  (let ((f (first xs)) (s (second xs)))
    (if (and f s)
	(if (eq f s) ; could count, how many elements are the same
	    (compress (rest xs))
	    (cons f (compress (rest xs)))))))


;; Problem 09.
(defun pack (xs &optional (acc ()))
  (if (not xs)
      acc
      (pack
       (rest xs)
       (if (eq (first xs) (first (first (last acc))))
	   (append (butlast acc) (list (append (first (last acc)) (list (first xs)))))
	   (append acc (list (list (first xs))))))))


;; Problem 10.
(defun encode (xs &optional (acc ()))
  (if (not xs)
      acc
      (encode
       (rest xs)
       (let ((counter (first (first (last acc))))
	     (symbol (second (first (last acc)))))
	 (if (eq (first xs) symbol)
	     (append (butlast acc) (list (list (1+ counter) symbol)))
	     (append acc (list (list 1 (first xs)))))))))
