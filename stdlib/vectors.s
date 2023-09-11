
; Standard vector library

(in-package 'spartan.core)

(defun vector-map! (f v)
  (let [(i 0) (n (length v))]
    (while (< i n)
      (set! (v i) (f (v i)))
      (inc! i))))

(defun vector-map (f v)
  (let [(result (copy v))]
    (map! f result)
    result))

(defun vector->list (v)
  (let [(i (- (length v) 1)) (result ())]
    (while (>= i 0)
      (set! result (cons (v i) result))
      (dec! i))
    result))

(defun vector-for-each (f v)
  (let [(i 0) (n (length v))]
    (while (< i n)
      (f (v i))
      (inc! i))))

(defun vector-for-each/index (f v)
  (let [(i 0) (n (length v))]
    (while (< i n)
      (f i (v i))
      (inc! i))))

(defun vector-filter (f v)
  (let [(result (vector))]
    (vector-for-each (fun (x) (if (f x) (vector-append! result x))) v)
    result))

(defun vector-reduce (f e v)
  (let [(result e)]
    (vector-for-each (fun (x) (set! result (f result x))) v)
    result))

(defun vector-swap! (v i j)
  (let ((temp (v i)))
    (set! (v i) (v j))
    (set! (v j) temp)))

(defun vector-reverse! (v)
  (let [(i 0) (j (- (length v) 1))]
    (while (< i j)
      (vector-swap! v i j)
      (inc! i)
      (dec! j))))

(defun vector-reverse (v)
  (let [(result (copy v))]
    (vector-reverse! result)
    result))

(defun vector-unfold (g n)
  (def result (make-vector n nil))
  (let [(i 0)]
    (while (< i n)
      (set! (result i) (g))
      (inc! i)))
  result)

; Sorts the elements of a vector, according to a given comparison function.
; The sort runs in O(N log N) time, where N is the number of elements.
; The comparison function determines the ordering amongst elements (i.e., implements a strict weak ordering on the element set).
; It receives two arguments and returns true if the first precedes the second, and false otherwise.
; The sort is stable: equivalent elements (according to the comparison function) maintain their original relative positions.

(defun vector-sort (compare v)
  
  (defun sort (source from to dest)    
    (if (> (- to from) 1)
      (let ((mid (quotient (+ from to) 2)))
        (sort source from mid dest)
        (sort source mid to dest)
        (merge source from mid to dest))))
  
  (defun merge (source from mid to dest)    
    (let ((i from) (j mid) (k from))
      
      (while (and (< i mid) (< j to))
        (let [(x (source i)) (y (source j))]
          (cond [(compare x y) (set! (dest k) x)
                               (inc! i)
                               (inc! k)]
                [else (set! (dest k) y)
                      (inc! j)
                      (inc! k)])))
      
      (while (< i mid)
        (set! (dest k) (source i))
        (inc! i)
        (inc! k))
      
      (while (< j to)
        (set! (dest k) (source j))
        (inc! j)
        (inc! k))
      
      (set! k from)
      (while (< k to)
        (set! (source k) (dest k))
        (inc! k))))
      
   (let* [(n (length v)) (dest (make-vector n nil))]
     (sort v 0 n dest)
     dest))
