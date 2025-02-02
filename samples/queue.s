
; Example implementation of a queue, making use of defstruct

; A queue is implemented here as a list containing the elements currently in the queue,
; and a (pointer to) the last element of that list (allowing constant-time push).

(in-ns queue)

(defrecord queue-type (front back))

(defun make-queue ()
  (make-queue-type () ()))

(defun queue? (self)
  (queue-type? self))

(defun empty? (self)
  (spartan.core:empty? (queue-type-front self)))

(defun push (self item)
  (let [(node (adjoin item ()))]
    (cond [(empty? self)
             (set-queue-type-front! self node)
             (set-queue-type-back! self node)]
          [else
             (set-rest! (queue-type-back self) node)
             (set-queue-type-back! self node)])))

(defun pop (self)
  (cond [(empty? self) #nil]
        [else
          (let [(node (queue-type-front self))]
            (set-queue-type-front! self (rest node))
            (if (spartan.core:empty? (queue-type-front self))
              (set-queue-type-back! self ()))
            (first node))]))


(in-ns user)
(import queue :as queue)
(def q (queue:make-queue))
(print-line "is queue? " (queue:queue? q))
(queue:push q 1)
(queue:push q 2)
(queue:push q 3)
(print-line "popped " (queue:pop q))
(print-line "popped " (queue:pop q))
(print-line "popped " (queue:pop q))
(print-line "queue empty? " (queue:empty? q))
