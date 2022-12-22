
; Standard vector library

(defun vector/map! (f v)
  (let ((i 0) (n (length v)))
    (while (< i n)
      (vector/set! v i (f (v i)))
      (set! i (+ 1 i)))))

(defun vector/map (f v)
  (let ((v2 (vector/copy v)))
    (vector/map! f v2)
    v2))

(defun vector/for-each (f v)
  (let ((i 0) (n (length v)))
    (while (< i n)
      (f (v i))
      (set! i (+ 1 i)))))

(defun vector/for-each-index (f v)
  (let ((i 0) (n (length v)))
    (while (< i n)
      (f i (v i))
      (set! i (+ 1 i)))))

(defun vector/filter (f v)
  (let ((v2 (vector)))
    (vector/for-each
      (fun (x)
        (if (f x)
          (vector/append! v2 x)))
      v)
    v2))

(defun vector/reduce (f e v)
  (let ((result e))
    (vector/for-each
      (fun (x)
        (set! result (f result x)))
      v)
    result))

(defun vector/swap! (v i j)
  (let ((temp (v i)))
    (vector/set! v i (v j))
    (vector/set! v j temp)))

(defun vector/reverse! (v)
  (let ((i 0) (j (- (length v) 1)))
    (while (< i j)
      (vector/swap! v i j)
      (set! i (+ 1 i))
      (set! j (- 1 j)))))

(defun vector/sort! (less-than? v)
  
  (defun sort (source from to temp)    
    (if (> (- to from) 1)
      (let ((mid (quotient (+ from to) 2)))
        (sort source from mid temp)
        (sort source mid to temp)
        (merge source from mid to temp))))
  
  (defun merge (source from mid to temp)
    
    (let ((i from) (j mid) (k from))
      
      (while (and (< i mid) (< j to))
        (cond ((less-than? (vector/ref source i)
                           (vector/ref source j))
                (vector/set! temp k (vector/ref source i))
                (set! i (+ 1 i)))
              (else
                (vector/set! temp k (vector/ref source j))
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
          (temp (vector/fill n nil)))
     (sort v 0 n temp)))
