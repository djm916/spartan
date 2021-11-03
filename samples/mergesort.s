
(defun vector/sort! (f v)
  
  (defun sort (source from to temp)
    (if (> (- to from) 1)
      (let ((mid (/ (+ from to) 2)))
        (sort source from mid temp)
        (sort source mid to temp)
        (merge source from mid to temp))))
  
  (defun merge (source from mid to temp)
    
    (let ((i from) (j mid) (k from))
      
      (while (and (< i mid) (< j to))
        (if (f (source i) (source j))
          (do (vector/set! temp k (source i))
              (set! i (+ 1 i)))
          (do (vector/set! temp k (source j))
              (set! j (+ 1 j))))
        (set! k (+ 1 k)))
      
      (while (< i mid)
        (vector/set! temp k (source i))
          (set! i (+ 1 i))
          (set! k (+ 1 k)))
      
      (while (< j to)
        (vector/set! temp k (source j))
          (set! j (+ 1 j))
          (set! k (+ 1 k)))
      
      (set! k from)
      (while (< k to)
        (vector/set! source k (temp k))
        (set! k (+ 1 k)))))
      
   (let* ((n (length v))
          (temp (vector/new n nil)))
     (sort v 0 n temp)))

(defun create-random-vector (n)
  (let ((v (vector/new n 0.0))
        (i 0))
    (while (< i n)
      (vector/set! v i (math/rand))
      (set! i (+ 1 i)))
    v))

(def v (create-random-vector 500000))

(vector/sort! < v)
