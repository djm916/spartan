
(use spartan.data.priority-queue :as queue)

(def N 20)

(def random-ints (vector-unfold (fun (i) (rand)) N))

(def q (queue:make-queue <))

(vector-foreach (fun (x) (queue:push q x)) random-ints)

(while (not (queue:empty? q))
  (print-line (format-decimal (queue:pop q))))
