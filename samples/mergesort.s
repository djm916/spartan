
(def N 20)

(def random-nums (vector-unfold (fun (i) (rand)) N))

(def sorted-nums (vector-sort < random-nums))

(vector-for-each (fun (x) (print (format-decimal x) " ")) sorted-nums)
