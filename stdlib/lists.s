
; Standard list processing library

; map - Maps a procedure across a list
;
; Given a procedure f and a list xs = (x1 x2 ... xN), (map f xs) returns a new list ((f x1) (f x2) ... (f xN))


(namespace core
  
  (defun map (f xs)
    (if (empty? xs) ()
      (cons (f (car xs))
            (map f (cdr xs)))))

  (defun map/index (f i xs)
    (if (empty? xs) () 
      (cons (f (car xs) i)
            (map/index f (+ 1 i) (cdr xs)))))

  (defun flat-map (f xs)
    (apply concat
           (map (fun (x) (map f x)) xs)))

  (defun for-each (f xs)
    (if (not (empty? xs))
      (do (f (car xs))
          (for-each f (cdr xs)))))

  (defun filter (f xs)
    (if (empty? xs) ()
      (if (f (car xs))
        (cons (car xs) (filter f (cdr xs)))
        (filter f (cdr xs)))))

  (defun filter! (f xs)
    (cond ((empty? xs)  ())
          ((f (car xs)) (set-cdr! xs (filter! f (cdr xs))) xs)
          (true         (filter! f (cdr xs)))))

  (defun find (f xs)
    (if (empty? xs) false
      (if (f (car xs))
        (car xs)
        (find f (cdr xs)))))

  (defun index (f xs)
    (rec loop ((i 0) (xs xs))
      (cond [(empty? xs)  -1]
            [(f (car xs)) i]
            [else         (loop (+ 1 i) (cdr xs))])))

  (defun contains? (x xs)
    (> (index (fun (y) (= x y)) xs) 0))

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
    
  (defun unfold-left (gen)
    (let ((next-elem (gen)))
      (if (nil? next-elem) ()
          (cons next-elem (unfold-left gen)))))

  (defun unfold-right (gen)
    (defun loop (result)
      (let ((next-elem (gen)))
        (if (nil? next-elem) result
            (loop (cons next-elem result)))))
    (loop ()))

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

  (defun compare (x y c)
    (cond [(and (empty? x) (empty? y)) 0]        ; x and y both empty, and all elements equal, so x and y are equal
          [(empty? x)                 -1]        ; x is empty, has fewer elements than y, so x is less than y
          [(empty? y)                 +1]        ; y is empty, has fewer elements than x, so x is greater than y
          [else                           
            (let ([order (c (car x) (car y))])   ; compare the first elements
              (if (/= order 0) order             ; the ordering of the first pair of unequal elements determines the result
                (compare (cdr x) (cdr y) c)))])) ; first elements equal, compare rest
))

 