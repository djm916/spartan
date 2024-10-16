
(in-package spartan.priority-queue)

(defrecord queue-type (comparator elems))

(defun make-priority-queue (comparator)
  (make-queue-type comparator (vector)))

(defun priority-queue? (self)
  (queue-type? self))

(defun empty? (self)
  (= 0 (vector-length (queue-type-elems self))))

(defun push (self item)
  (def v (queue-type-elems self))
  (def c (queue-type-comparator self))
  (vector-append! v item)
  (__reheap-up v c))

(defun __reheap-up (v c)
  (let [(root (- (vector-length v) 1))]
    (while (> root 0)
      (let [(parent (quotient (- root 1) 2))]
        (if (c (vector-ref v root) (vector-ref v parent))
          (vector-swap! v root parent)
          (set! root parent))))))

(defun pop (self)
  (def v (queue-type-elems self))
  (def c (queue-type-comparator self))
  (if (empty? self) void
    (let [(top (vector-ref v 0)) (last (- (vector-length v) 1))]
      (vector-set! v 0 (vector-ref v last))
      (vector-remove! v last)
      (__reheap-down v c)
      top)))

(defun __reheap-down (v c)
  (let [(root 0) (left 1) (right 2) (last (vector-length v)) (done #false)]
    (while (and (not done) (< left last))
      (let [(child (cond [(not (< right last)) left]
                         [(c (vector-ref v left) (vector-ref v right)) left]
                         [else right]))]
        (if (c (vector-ref v child) (vector-ref v root))
          (do
            (vector-swap! v root child)
            (set! root child)
            (set! left (+ 1 (* 2 root)))
            (set! right (+ 2 (* 2 root))))
          (set! done #true))))))
