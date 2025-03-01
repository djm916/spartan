(in-ns spartan.priority-queue.internal)

(defrecord queue (comparator elems))

(defun reheap-up (v c)
  (let [(root (- (vector-length v) 1))]
    (while (> root 0)
      (let [(parent (quotient (- root 1) 2))]
        (if (c (vector-ref v root) (vector-ref v parent))
          (vector-swap! v root parent)
          (set! root parent))))))

(defun reheap-down (v c)
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

(in-ns spartan.priority-queue)
(import spartan.priority-queue.internal :as internal)

(defun make-priority-queue (comparator)
  (internal:make-queue comparator (vector)))

(defun priority-queue? (self)
  (internal:queue? self))

(defun empty? (self)
  (= 0 (vector-length (internal:queue-elems self))))

(defun push (self item)
  (def v (internal:queue-elems self))
  (def c (internal:queue-comparator self))
  (vector-append! v item)
  (internal:reheap-up v c))

(defun pop (self)
  (def v (internal:queue-elems self))
  (def c (internal:queue-comparator self))
  (if (empty? self) void
    (let [(top (vector-ref v 0)) (last (- (vector-length v) 1))]
      (vector-set! v 0 (vector-ref v last))
      (vector-remove! v last)
      (internal:reheap-down v c)
      top)))
