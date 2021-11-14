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


;; Problem 11.
(defun encode-modified (xs &optional (acc ()))
  (if (not xs)
      acc
      (encode-modified
       (rest xs)
       (let ((last-elem (first (last acc))))
	 (if (listp last-elem)
	     (let ((counter (first last-elem))
		   (symbol (second last-elem)))
	       (if (eq (first xs) symbol)
		   (append (butlast acc) (list (list (1+ counter) symbol)))
		   (append acc (list (first xs)))))
	     (if (eq (first xs) last-elem)
		 (append (butlast acc) (list (list 2 (first xs))))
		 (append acc (list (first xs)))))))))


;; Problem 12.
(defun decode (xs &optional (acc ()))
  (if (not xs)
      acc
      (decode
       (rest xs)
       (append acc
	       (if (listp (first xs))
		   (let ((counter (first (first xs)))  ; apply destructuring someday
			 (symbol (second (first xs))))
		     (make-list counter :initial-element symbol))
		   (list (first xs)))))))

;; Problem 13.
; todo

;; Problem 14.
(defun dupli (xs &optional (acc ()))
  (if (not xs)
      acc
      (dupli (rest xs) (append acc (list (first xs) (first xs))))))


;; Problem 15.
(defun repli (xs n &optional (acc ()))
  (if (not xs)
      acc
      (repli
       (rest xs)
       (append acc (make-list n :initial-element (first xs))))))



;; Problem 16.
(defun drop (xs n &optional (acc ()) (nn n))
  (if (not xs)
      acc
      (if (eq (1- nn) 0)
	  (drop (rest xs) n acc n)
	  (drop (rest xs) n (append acc (list (first xs))) (1- nn)))))


;; Problem 17.
(defun split (xs n &optional (acc ()))
  (if (not xs)
      acc
      (if (eq n 0)
	  (list acc xs)
	  (split (rest xs) (1- n) (append acc (list (first xs)))))))


;; Probiem 18.
(defun slice (xs start end &optional (acc ()) (counter 1))
  ;; should assert that start is lower than end
  (cond
    ((< counter start)
     (slice (rest xs) start end acc (1+ counter)))
    ((and (>= counter start) (<= counter end))
     (slice (rest xs) start end (append acc (list (first xs))) (1+ counter)))
    (t acc)))


;; Problem 19.
;; todo

;; Problem 20.
(defun remove-at (xs n &optional (acc ()) (counter 1))
  (if (not xs)
      acc
      (if (< counter n)
	  (remove-at (rest xs) n (append acc (list (first xs))) (1+ counter))
	  (append acc (rest xs)))))


;; Problem 21.
(defun insert-at (symbol xs n &optional (acc ()) (counter 1))
  (if (< counter n)
      (insert-at symbol (rest xs) n (append acc (list (first xs))) (1+ counter))
      (append acc (list symbol) xs)))


;; Well, after all of that, I can see that most of my solutions
;; has been implemented using accumulator
;; so it might be a chance to implement macro for extended
;; `defun` definition
;; Could closure might be helpful?

;; Problem 22.
(defun range (start end &optional (acc ()) (counter start))
  (if (<= start end)
      (if (<= counter end)
	  (range start end (append acc (list counter)) (1+ counter))
	  acc)
      (if (>= counter end)
	  (range start end (append acc (list counter)) (1- counter))
	  acc)))
