
; Example of ADT "queue", making use of "defstruct" macro

(require "stdlib/defstruct.s")

(defstruct __queue-impl (front back))

(defun queue ()
  (__queue-impl () ()))

(defun queue? (self)
  (__queue-impl? self))

(defun queue/empty? (self)
  (and (empty? (__queue-impl/front self)) 
       (empty? (__queue-impl/back self))))

(defun queue/push (self item)
  (let ((node (cons item ())))
    (cond ((queue/empty? self)
             (__queue-impl/set-front! self node)
             (__queue-impl/set-back! self node))
          (else
             (set-cdr! (__queue-impl/back self) node)
             (__queue-impl/set-back! self node)))))

(defun queue/pop (self)
  (cond ((queue/empty? self) nil)
        (else
          (let ((node (__queue-impl/front self)))
            (__queue-impl/set-front! self (cdr node))
            (if (empty? (__queue-impl/front self))
              (__queue-impl/set-back! self ()))
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
