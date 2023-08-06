
; Standard vector library

(defun vector/map! (f v)
  (let ((i 0) (n (length v)))
    (while (< i n)
      (set-at! v i (f (v i)))
      (inc! i))))

(defun vector/map (f v)
  (let ((v2 (vector/copy v)))
    (vector/map! f v2)
    v2))

(defun vector->list (v)
  (let ((i (- (length v) 1))
        (result ()))
    (while (>= i 0)
      (set! result (cons (at v i) result))
      (dec! i))
    result))

(defun vector/for-each (f v)
  (let ((i 0) (n (length v)))
    (while (< i n)
      (f (v i))
      (inc! i))))

(defun vector/for-each-index (f v)
  (let ((i 0) (n (length v)))
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
    (set-at! v i (v j))
    (set-at! v j temp)))

(defun vector/reverse! (v)
  (let ((i 0) (j (- (length v) 1)))
    (while (< i j)
      (vector/swap! v i j)
      (inc! i)
      (dec! j))))

(defun vector/unfold (g n)
  (def v (vector/make n nil))
  (let ((i 0))
    (while (< i n)
      (set-at! v i (g))
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
        (let ((x (at source i))
              (y (at source j)))
          (cond ((<? x y) (set-at! temp k x)
                          (inc! i)
                          (inc! k))
                (else     (set-at! temp k y)
                          (inc! j)
                          (inc! k)))))
      
      (while (< i mid)
        (set-at! temp k (at source i))
        (inc! i)
        (inc! k))
      
      (while (< j to)
        (set-at! temp k (at source j))
        (inc! j)
        (inc! k))
      
      (set! k from)
      (while (< k to)
        (set-at! source k (at temp k))
        (inc! k))))
      
   (let* ((n (length v))
          (temp (vector/make n nil)))
     (sort v 0 n temp)))
