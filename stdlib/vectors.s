
; Standard vector library

(defun vector/map! (f xs)
  (let ((n (length xs))
        (i 0))
    (while (< i n)
      (vector/set! xs i (f (xs i)))
      (set! i (+ 1 i)))))

(defun vector/map (f xs)
  (def xs2 (vector/copy xs))
  (vector/map! f xs2)
  xs2)

(defun vector/for-each (f xs)
  (let* ((n (length xs))
         (i 0))
    (while (< i n)
      (f (xs i))
      (set! i (+ 1 i)))))

(defun vector/filter (f xs)
  (let* ((result (vector))
         (n (length xs))
         (i 0))
    (while (< i n)
      (if (f (xs i))
        (vector/append! result (xs i)))
      (set! i (+ 1 i)))
    result))

(defun vector/reduce (f i xs)
  (let* ((result i)
         (n (length xs))
         (i 0))
    (while (< i n)
      (set! result (f result (xs i)))
      (set! i (+ 1 i)))
    result))

(defun vector/sort! (f v)
  
  (defun sort (source from to temp)
    (if (> (- to from) 1)
      (let ((mid (quotient (+ from to) 2)))
        (sort source from mid temp)
        (sort source mid to temp)
        (merge source from mid to temp))))
  
  (defun merge (source from mid to temp)
    
    (let ((i from) (j mid) (k from))
      
      (while (and (< i mid) (< j to))
        (if (f (vector/ref source i) (vector/ref source j))
          (do (vector/set! temp k (source i))
              (set! i (+ 1 i)))
          (do (vector/set! temp k (source j))
              (set! j (+ 1 j))))
        (set! k (+ 1 k)))
      
      (while (< i mid)
        (vector/set! temp k (vector/ref source i))
        (set! i (+ 1 i))
        (set! k (+ 1 k)))
      
      (while (< j to)
        (vector/set! temp k (vector/ref source j))
        (set! j (+ 1 j))
        (set! k (+ 1 k)))
      
      (set! k from)
      (while (< k to)
        (vector/set! source k (vector/ref temp k))
        (set! k (+ 1 k)))))
      
   (let* ((n (length v))
          (temp (vector/new n nil)))
     (sort v 0 n temp)))
