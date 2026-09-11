
; Example module implementing a simple queue data structure

(in-module queue)

(export queue
        queue?
        empty?
        push
        pop)
        
; A queue is implemented here as a list containing the elements currently in the queue,
; and a (pointer to) the last element of that list (allowing constant-time push).

(defrecord Queue
  queue
  queue?
  (front queue-front queue-set-front!)
  (back queue-back queue-set-back!))

(let ((orig queue))
  (set! queue (fun () (orig () ()))))

(defun empty? (self)
  (spartan.base:empty? (queue-front self)))

(defun push (self item)
  (let [(node (adjoin item ()))]
    (cond [(empty? self)
           (queue-set-front! self node)
           (queue-set-back! self node)]
          [else
           (set-rest! (queue-back self) node)
           (queue-set-back! self node)])))

(defun pop (self)
  (cond [(empty? self) #nil]
        [else
         (let [(node (queue-front self))]
           (queue-set-front! self (rest node))
           (if (spartan.base:empty? (queue-front self))
             (queue-set-back! self ()))
           (first node))]))

(in-module user)
;(import queue :as queue)
(println "the module queue exports the symbols: " (module-symbols (the-module 'queue)))
(def q (queue:queue))
(println "is queue? " (queue:queue? q))
(queue:push q 1)
(queue:push q 2)
(queue:push q 3)
(println "popped " (queue:pop q))
(println "popped " (queue:pop q))
(println "popped " (queue:pop q))
(println "queue empty? " (queue:empty? q))
