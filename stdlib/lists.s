
(defun map (f xs)
  (if (empty? xs) ()
    (cons (f (car xs))
          (map f (cdr xs)))))

(defun map-with-index (f i xs)
  (if (empty? xs) () 
    (cons (f (car xs) i)
          (map-with-index f (+ 1 i) (cdr xs)))))

(defun flat-map (f xs)
  (apply concat
         (map (fun (x) (map f x)) xs)))

(defun filter (f xs)
  (if (empty? xs) ()
    (if (f (car xs))
      (cons (car xs) (filter f (cdr xs)))
      (filter f (cdr xs)))))

(defun filter! (f xs)
  (cond ((empty? xs)  ())
        ((f (car xs)) (set-cdr! xs (filter! f (cdr xs))) xs)
        (true         (filter! f (cdr xs)))))

(defun remove (f xs)
  (filter (fun (x) (not (f x))) xs))

(defun remove! (f xs)
  (filter! (fun (x) (not (f x))) xs))

; Reduce a list to a single value

; Inputs:
;   f     a binary function
;   xs:   a list of elements x1 x2 ... xN
;   i:    an inital value
    
; For example, a 3 element list reduces as:
;
;   (f (f (f i x1) x2) x3)

(defun fold-left (f i xs)
  (if (empty? xs) i
    (fold-left f (f i (car xs)) (cdr xs))))

; Reduce a list to a single value

; Inputs:
;   f     a binary function
;   xs:   a list of elements x1 x2 ... xN
;   i:    an inital value
    
; For example, a 3 element list reduces as:
;
;   (f x1 (f x2 (f x3 i)))

(defun fold-right (f i xs)
  (if (empty? xs) i
    (f (car xs) (fold-right f i (cdr xs)))))
  
(defun range (i j)
  (if (> i j) ()
    (cons i (range (+ 1 i) j))))

; Note: defined in core
;(defun reverse (xs)
;  (defun loop (xs sx)
;    (if (empty? xs) sx
;      (loop (cdr xs) (cons (car xs) sx))))
;  (loop xs ()))

(defun enumerate (xs i)
  (if (empty? xs) ()
    (cons (list i (car xs))
          (enumerate (cdr xs) (+ 1 i)))))

(defun take (n xs)
  (if (= 0 n) ()
    (cons (car xs) (take (- n 1) (cdr xs)))))

(defun take-while (f xs)
  (if (empty? xs) ()
    (if (f (car xs))
      (cons (car xs) (take-while f (cdr xs)))
      ())))

(defun drop (n xs)
  (if (= 0 n) xs
    (drop (- n 1) (cdr xs))))

(defun drop-while (f xs)
  (if (empty? xs) xs
    (if (f (car xs))
      (drop-while f (cdr xs))
      xs)))

(defun zip (f xs ys)
  (if (or (empty? xs) (empty? ys)) ()
    (cons (f (car xs) (car ys))
          (zip f (cdr xs) (cdr ys)))))
