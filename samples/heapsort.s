
(load "stdlib/priority-queue.s")

(import spartan.priority-queue :as pq)

(def N 20)

(def random-ints (vector-unfold (fun (i) (rand)) N))

(def q (pq:make-priority-queue <))

(vector-for-each (fun (x) (pq:push q x)) random-ints)

(while (not (pq:empty? q))
  (print-line (format-decimal (pq:pop q))))
