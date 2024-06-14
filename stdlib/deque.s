; A deque (double-ended queue) is a flexible vector-like data structure.
; It supports constant time element access. Insertion at either end of the queue
; happens in amortized constant time. 

(in-package spartan.deque)

(defrecord deque-type (elems front back size))

(defun deque ()
  (def initial-capacity 4)
  (make-deque-type (make-vector initial-capacity void) 0 0 0))

(def deque? deque-type?)

(defun push-back (self item)
  (def elems (queue-type-elems self))
  (def capacity (vector-length elems))
  (def size (deque-type-size self))
  (if (<= capacity (+ 1 size))
    (push-back (__resize self) item))
  (vector-set! elems back item)
  (deque-type-back-set! self (remainder (+ 1 (deque-type-back self)) (deque-type-