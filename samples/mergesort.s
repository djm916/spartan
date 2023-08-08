
(require "stdlib/vectors.s")

(def N 20)

(def v (vector/unfold rand N)) ; Create an array of random integers

(vector/sort! < v) ; Sort the vector

(vector/for-each v (fun (x) (print (format-decimal x) " ")))
