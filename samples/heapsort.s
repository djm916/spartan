
(require "stdlib/priority-queue.s")

(def N 20)

(def random-ints (vector-unfold rand N))

(def q (spartan.priority-queue:priority-queue <))

(vector-for-each (fun (x) (spartan.priority-queue:push q x)) random-ints)

(while (not (spartan.priority-queue:empty? q))
  (print-line (format-decimal (spartan.priority-queue:pop q))))
