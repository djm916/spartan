
;(require "stdlib/vectors.s")

; level  row
; ----------------
; 0      [1]      
; 1      [1 1]    
; 2      [1 2 1]  
; 3      [1 3 3 1]
;-----------------
; index   0 1 2 3

; length of row on level L is equal to L + 1
; each row begins and ends with a 1
; the ith element of a row on level L

; Generate Pascal's Triangle
; The triangle is represented as a vector of vectors. Each element of
; the outer vector represents a of row of the triangle.
(defun pascals-triangle (height)
  (def triangle (vector (vector 1)))
  (def level 1)  
  (while (< level height)
    (let* ((row-above (triangle (- level 1)))
           (row (generate-row row-above)))
      (vector-append! triangle row)
      (inc! level)))
  triangle)

(defun generate-row (row-above)
  (def len-above (length row-above))
  (def len (+ 1 len-above))
  (def row (make-vector len nil))
  (def i 1)
  (set! (row 0) 1)
  (while (< i len-above)
    (let ((a (row-above (- i 1)))
          (b (row-above i)))
      (set! (row i) (+ a b)))
      (inc! i))
  (set! (row (- (length row) 1)) 1)
  row)

(defun print-row (row)
  (vector-for-each (fun (n) (print n " ")) row)
  (print-line))

(defun print-triangle (triangle)
  (vector-for-each print-row triangle))

(print-triangle (pascals-triangle 5))
