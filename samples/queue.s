
; Example of ADT "queue", making use of "defstruct" macro

(require "stdlib/defstruct.s")

(defstruct queue (front back))

(defun queue/new ()
  (make-queue () ()))

(defun queue/empty? (q)
  (and (empty? (queue-front q)) 
       (empty? (queue-back q))))

(defun queue/push (q x)
  (let ((node (cons x ())))
    (cond ((queue/empty? q)
             (set-queue-front! q node)
             (set-queue-back! q node))
          (else
             (set-cdr! (queue-back q) node)
             (set-queue-back! q node)))))

(defun queue/pop (q)
  (cond ((queue/empty? q) nil)
        (else
          (let ((node (queue-front q)))
            (set-queue-front! q (cdr node))
            (if (empty? (queue-front q))
              (set-queue-back! q ()))
            (car node)))))

(def q (queue/new))
(queue/push q 1)
(queue/push q 2)
(queue/push q 3)
(print-line "popped " (queue/pop q))
(print-line "popped " (queue/pop q))
(print-line "popped " (queue/pop q))
(print-line "queue empty? " (queue/empty? q))
