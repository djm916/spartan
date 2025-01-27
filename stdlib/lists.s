
; Standard list processing library

(in-package spartan.core)

; map - Maps a procedure across a list
;
; Given a procedure f and a list xs = (x1 x2 ... xN), (map f xs) returns a new list ((f x1) (f x2) ... (f xN))
(defun map (f xs)
  (if (empty? xs) ()
    (adjoin (f (first xs))
            (map f (rest xs)))))

(defun map/index (f i xs)
  (if (empty? xs) ()
    (adjoin (f (first xs) i)
            (map/index f (+ 1 i) (rest xs)))))

(defun for-each (f xs)
  (if (not (empty? xs))
    (do (f (first xs))
        (for-each f (rest xs)))))

(defun filter (f xs)
  (if (empty? xs) ()
    (if (f (first xs))
      (adjoin (first xs) (filter f (rest xs)))
      (filter f (rest xs)))))

(defun find (f xs)
  (if (empty? xs) #nil
    (if (f (first xs))
      (first xs)
      (find f (rest xs)))))

(defun find-index (f xs)
  (rec loop [(i 0) (xs xs)]
    (cond [(empty? xs)    -1]
          [(f (first xs))  i]
          [else (loop (+ 1 i) (rest xs))])))

(defun contains? (x xs)
  (not (nil? (find (fun (y) (= x y)) xs))))

(defun remove (f xs)
  (filter (fun (x) (not (f x))) xs))

; Reduce a list to a single value by applying a binary function to successive
; elements of the list (from the left).
;
; Parameters:
;   f    a binary function
;   i    an inital value
;   xs   a list of elements, (x1 x2 ... xN)
;
; (fold-left f i xs) ==> (f ... (f (f i x1) x2) ... xN)
;
; For example,
; 
; (fold-left + 0 '(1 2 3)) ==> (+ (+ (+ 0 1) 2) 3) ==> 6

(defun fold-left (f i xs)
  (if (empty? xs) i
    (fold-left f (f i (first xs)) (rest xs))))

; Reduce a list to a single value by applying a binary function to successive
; elements of the list (from the right).
;
; Parameters:
;   f    a binary function
;   i    an inital value
;   xs   a list of elements, (x1 x2 ... xN)
;
; (fold-right f i xs) ==> (f x1 (f x2 ... (f xN i)))
;
; For example,
;
; (fold-right cons () '(1 2 3)) ==> (cons 1 (cons 2 (cons 3 ()))) ==> (1 2 3)

(defun fold-right (f i xs)
  (if (empty? xs) i
    (f (first xs) (fold-right f i (rest xs)))))

(defun unfold-left (gen)
  (let ((next-elem (gen)))
    (if (nil? next-elem) ()
        (adjoin next-elem (unfold-left gen)))))

(defun unfold-right (gen)
  (defun loop (result)
    (let ((next-elem (gen)))
      (if (nil? next-elem) result
          (loop (adjoin next-elem result)))))
  (loop ()))

(defun enumerate (i xs)
  (if (empty? xs) ()
    (adjoin (list i (first xs))
            (enumerate (+ 1 i) (rest xs)))))

(defun take (n xs)
  (if (= 0 n) ()
    (adjoin (first xs) (take (- n 1) (rest xs)))))

(defun take-while (f xs)
  (if (empty? xs) ()
    (if (f (first xs))
      (adjoin (first xs) (take-while f (rest xs)))
      ())))

(defun drop (n xs)
  (if (= 0 n) xs
    (drop (- n 1) (rest xs))))

(defun drop-while (f xs)
  (if (empty? xs) xs
    (if (f (first xs))
      (drop-while f (rest xs))
      xs)))

(defun zip (f xs ys)
  (if (or (empty? xs) (empty? ys)) ()
    (adjoin (f (first xs) (first ys))
            (zip f (rest xs) (rest ys)))))

(defun list-compare (x y c)
  (cond [(and (null? x) (null? y)) 0]        ; x and y both empty, and all elements equal, so x and y are equal
        [(null? x)                 -1]        ; x is empty, has fewer elements than y, so x is less than y
        [(null? y)                 +1]        ; y is empty, has fewer elements than x, so x is greater than y
        [else                           
          (let ([order (c (car x) (car y))])   ; compare the first elements
            (if (/= order 0) order             ; the ordering of the first pair of unequal elements determines the result
              (list-compare (cdr x) (cdr y) c)))])) ; first elements equal, compare rest
