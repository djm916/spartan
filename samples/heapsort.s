
(use spartan.data.priority-queue :as queue)

(def N 20)

(def random-ints (vector-unfold (fun (i) (rand)) N))

(def q (queue:queue <))

(vector-foreach (fun (x) (queue:push q x)) random-ints)

(while (not (queue:empty? q))
  (println (format-decimal (queue:pop q))))
