
; Standard vector library

(defun vector/map! (f v)
  (let ((i 0) (n (vector/length v)))
    (while (< i n)
      (vector/set! v i (f (vector/ref v i)))
      (inc! i))))

(defun vector/map (f v)
  (let ((v2 (vector/copy v)))
    (vector/map! f v2)
    v2))

(defun vector->list (v)
  (let ((i (- (vector/length v) 1))
        (result ()))
    (while (>= i 0)
      (set! result (cons (vector/ref v i) result))
      (dec! i))
    result))

(defun vector/for-each (v f)
  (let ((i 0) (n (vector/length v)))
    (while (< i n)
      (f (vector/ref v i))
      (inc! i))))

(defun vector/for-each-index (f v)
  (let ((i 0) (n (vector/length v)))
    (while (< i n)
      (f i (v i))
      (inc! i))))

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
  (let ((i 0) (j (- (vector/length v) 1)))
    (while (< i j)
      (vector/swap! v i j)
      (inc! i)
      (dec! j))))

(defun vector/unfold (g n)
  (def v (vector/make n nil))
  (let ((i 0))
    (while (< i n)
      (vector/set! v i (g))
      (inc! i)))
    v)

; Sorts the elements of a vector, according to a given comparison function.
; The sort runs in O(N log N) time, where N is the number of elements.
; The comparison function determines the ordering amongst elements (i.e., implements a strict weak ordering on the element set).
; It receives two arguments and returns true if the first precedes the second, and false otherwise.
; The sort is stable: equivalent elements (according to the comparison function) maintain their original relative positions.

(defun vector/sort! (<? v)
  
  (defun sort (source from to temp)    
    (if (> (- to from) 1)
      (let ((mid (quotient (+ from to) 2)))
        (sort source from mid temp)
        (sort source mid to temp)
        (merge source from mid to temp))))
  
  (defun merge (source from mid to temp)    
    (let ((i from) (j mid) (k from))
      
      (while (and (< i mid) (< j to))
        (let ((x (vector/ref source i))
              (y (vector/ref source j)))
          (cond ((<? x y) (vector/set! temp k x)
                          (inc! i)
                          (inc! k))
                (else     (vector/set! temp k y)
                          (inc! j)
                          (inc! k)))))
      
      (while (< i mid)
        (vector/set! temp k (vector/ref source i))
        (inc! i)
        (inc! k))
      
      (while (< j to)
        (vector/set! temp k (vector/ref source j))
        (inc! j)
        (inc! k))
      
      (set! k from)
      (while (< k to)
        (vector/set! source k (vector/ref temp k))
        (inc! k))))
      
   (let* ((n (vector/length v))
          (temp (vector/make n nil)))
     (sort v 0 n temp)))
