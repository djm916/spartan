
(require "stdlib/vectors.s")

(defun create-random-vector (n)
  (let ((v []) (i 0))
    (while (< i n)
      (vector/append! v (rand))
      (set! i (+ 1 i)))
    v))

(def v (create-random-vector 25))

(vector/sort! < v)

(vector/for-each print-line (vector/map (fun (x) (format-decimal x 3)) v))
