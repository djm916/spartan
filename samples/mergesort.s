
(require "stdlib/vectors.s")

(import 'spartan.vector)

(def N 20)

(def random-nums (unfold rand N))

(def sorted-nums (sort < random-nums))

(for-each (fun (x) (print (format-decimal x) " ")) sorted-nums)
