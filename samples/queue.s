
; Example implementation of a queue, making use of defstruct

; A queue is implemented here as a list containing the elements currently in the queue,
; and a (pointer to) the last element of that list (allowing constant-time push).

(require "stdlib/defstruct.s")

(defstruct __queue (front back))

(defun queue ()
  (__queue () ()))

(defun queue? (self)
  (__queue? self))

(defun queue/empty? (self)
  (empty? (__queue/front self)))

  ;(and (empty? (__queue/front self)) 
   ;    (empty? (__queue/back self))))

(defun queue/push (self item)
  (let ((node (cons item ())))
    (cond ((queue/empty? self)
             (__queue/set-front! self node)
             (__queue/set-back! self node))
          (else
             (set-cdr! (__queue/back self) node)
             (__queue/set-back! self node)))))

(defun queue/pop (self)
  (cond ((queue/empty? self) nil)
        (else
          (let ((node (__queue/front self)))
            (__queue/set-front! self (cdr node))
            (if (empty? (__queue/front self))
              (__queue/set-back! self ()))
            (car node)))))

(def q (queue))
(print-line "is queue? " (queue? q))
(queue/push q 1)
(queue/push q 2)
(queue/push q 3)
(print-line "popped " (queue/pop q))
(print-line "popped " (queue/pop q))
(print-line "popped " (queue/pop q))
(print-line "queue empty? " (queue/empty? q))
