
(require "stdlib/vectors.s")

(def N 50)

(def v (vector/unfold rand N)) ; Create an array of random integers

(vector/sort! < v) ; Sort the vector

(print v "\n")
