
; Example implementation of a queue, making use of defstruct

; A queue is implemented here as a list containing the elements currently in the queue,
; and a (pointer to) the last element of that list (allowing constant-time push).

(defrecord queue-type (front back))

(defun queue ()
  (make-queue-type () ()))

(defun queue? (self)
  (queue-type? self))

(defun empty? (self)
  (null? (queue-type-front self)))

(defun push (self item)
  (let [(node (cons item ()))]
    (cond [(empty? self)
             (queue-type-set-front! self node)
             (queue-type-set-back! self node)]
          [else
             (set-cdr! (queue-type-back self) node)
             (queue-type-set-back! self node)])))

(defun pop (self)
  (cond [(empty? self) void]
        [else
          (let [(node (queue-type-front self))]
            (queue-type-set-front! self (cdr node))
            (if (null? (queue-type-front self))
              (queue-type-set-back! self ()))
            (car node))]))

(def q (queue))
(print-line "is queue? " (queue? q))
(push q 1)
(push q 2)
(push q 3)
(print-line "popped " (pop q))
(print-line "popped " (pop q))
(print-line "popped " (pop q))
(print-line "queue empty? " (empty? q))
