
(require "stdlib/vectors.s")

(defun create-random-vector (n)
  (let ((v (vector/fill n 0.0))
        (i 0))
    (while (< i n)
      (vector/set! v i (rand))
      (set! i (+ 1 i)))
    v))

(def v (create-random-vector 50))

(vector/sort! < v)

(print-line v)
