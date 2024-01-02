
(in-package spartan.priority-queue)

(defstruct queue-impl (comparator vector))

(defun make-priority-queue (comparator)
  (make-queue-impl comparator (vector)))

(defun priority-queue? (self)
  (queue-impl? self))

(defun empty? (self)
  (= 0 (length (self 'vector))))

(defun push (self item)
  (def v (self 'vector))
  (def c (self 'comparator))
  (vector-append! v item)
  (__reheap-up v c))

(defun __reheap-up (v c)
  (let [(root (- (length v) 1))]
    (while (> root 0)
      (let [(parent (quotient (- root 1) 2))]
        (if (c (v root) (v parent))
          (vector-swap! v root parent)
          (set! root parent))))))

(defun pop (self)
  (def v (self 'vector))
  (def c (self 'comparator))
  (if (empty? self) nil
    (let [(top (v 0)) (last (- (length v) 1))]
      (set! (v 0) (v last))
      (vector-remove! v last)
      (__reheap-down v c)
      top)))

(defun __reheap-down (v c)
  (let [(root 0) (left 1) (right 2) (last (length v)) (done false)]
    (while (and (not done) (< left last))
      (let [(child (cond [(not (< right last)) left]
                         [(c (v left) (v right)) left]
                         [else right]))]
        (if (c (v child) (v root))
          (begin
            (vector-swap! v root child)
            (set! root child)
            (set! left (+ 1 (* 2 root)))
            (set! right (+ 2 (* 2 root))))
          (set! done true))))))
