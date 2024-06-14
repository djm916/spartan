
; Standard list processing library

; map - Maps a procedure across a list
;
; Given a procedure f and a list xs = (x1 x2 ... xN), (map f xs) returns a new list ((f x1) (f x2) ... (f xN))

(in-package spartan.core)

(defun list-empty? (x)
  (and (list? x) (not (null? x))))

(defun list-ref (xs i)
  (car (list-drop i xs)))

(defun list-set! (xs i e)
  (set-car! (list-drop i xs)))

(defun list-concat (xs ys)
  (if (null? xs) ys
    (cons (car xs) (list-concat (cdr xs) ys))))

(defun list-append (xs e)
  (list-concat xs (list e)))

(defun list-map (f xs)
  (if (null? xs) ()
    (cons (f (car xs))
          (list-map f (cdr xs)))))

(defun list-map/index (f i xs)
  (if (null? xs) () 
    (cons (f (car xs) i)
          (list-map/index f (+ 1 i) (cdr xs)))))

(defun list-flat-map (f xs)
  (apply list-concat
         (list-map (fun (xs) (list-map f xs)) xs)))

(defun list-for-each (f xs)
  (if (not (null? xs))
    (do (f (car xs))
        (list-for-each f (cdr xs)))))

(defun list-filter (f xs)
  (if (null? xs) ()
    (if (f (car xs))
      (cons (car xs) (list-filter f (cdr xs)))
      (list-filter f (cdr xs)))))

(defun list-filter! (f xs)
  (cond [(null? xs)    ()]
        [(f (car xs))  (set-cdr! xs (filter! f (cdr xs))) xs]
        [else          (list-filter! f (cdr xs))]))

(defun list-find (f xs)
  (if (null? xs) void
    (if (f (car xs))
      (car xs)
      (list-find f (cdr xs)))))

(defun list-find-index (f xs)
  (rec loop [(i 0) (xs xs)]
    (cond [(null? xs)    -1]
          [(f (car xs))  i]
          [else          (loop (+ 1 i) (cdr xs))])))

(defun list-contains? (x xs)
  (>= (list-find-index (fun (y) (= x y)) xs) 0))

(defun list-remove (f xs)
  (list-filter (fun (x) (not (f x))) xs))

(defun list-remove! (f xs)
  (list-filter! (fun (x) (not (f x))) xs))

; Reduce a list to a single value

; Inputs:
;   f     a binary function
;   xs:   a list of elements x1 x2 ... xN
;   i:    an inital value
    
; For example, a 3 element list reduces as:
;
;   (f (f (f i x1) x2) x3)

(defun list-fold-left (f i xs)
  (if (null? xs) i
    (list-fold-left f (f i (car xs)) (cdr xs))))

; Reduce a list to a single value

; Inputs:
;   f     a binary function
;   xs:   a list of elements x1 x2 ... xN
;   i:    an inital value
    
; For example, a 3 element list reduces as:
;
;   (f x1 (f x2 (f x3 i)))

(defun list-fold-right (f i xs)
  (if (null? xs) i
    (f (car xs) (list-fold-right f i (cdr xs)))))

(defun list-unfold-left (gen)
  (let ((next-elem (gen)))
    (if (void? next-elem) ()
        (cons next-elem (list-unfold-left gen)))))

(defun list-unfold-right (gen)
  (defun loop (result)
    (let ((next-elem (gen)))
      (if (nil? next-elem) result
          (loop (cons next-elem result)))))
  (loop ()))

(defun list-enumerate (xs i)
  (if (null? xs) ()
    (cons (list i (car xs))
          (list-enumerate (cdr xs) (+ 1 i)))))

(defun list-take (n xs)
  (if (= 0 n) ()
    (cons (car xs) (list-take (- n 1) (cdr xs)))))

(defun list-take-while (f xs)
  (if (null? xs) ()
    (if (f (car xs))
      (cons (car xs) (list-take-while f (cdr xs)))
      ())))

(defun list-drop (n xs)
  (if (= 0 n) xs
    (list-drop (- n 1) (cdr xs))))

(defun list-drop-while (f xs)
  (if (null? xs) xs
    (if (f (car xs))
      (list-drop-while f (cdr xs))
      xs)))

(defun list-zip (f xs ys)
  (if (or (null? xs) (null? ys)) ()
    (cons (f (car xs) (car ys))
          (list-zip f (cdr xs) (cdr ys)))))

(defun list-compare (x y c)
  (cond [(and (null? x) (null? y)) 0]        ; x and y both empty, and all elements equal, so x and y are equal
        [(null? x)                 -1]        ; x is empty, has fewer elements than y, so x is less than y
        [(null? y)                 +1]        ; y is empty, has fewer elements than x, so x is greater than y
        [else                           
          (let ([order (c (car x) (car y))])   ; compare the first elements
            (if (/= order 0) order             ; the ordering of the first pair of unequal elements determines the result
              (list-compare (cdr x) (cdr y) c)))])) ; first elements equal, compare rest
