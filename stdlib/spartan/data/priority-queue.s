(in-module spartan.data.priority-queue)

(export make-queue
        queue?
        empty?
        push
        pop)

(defrecord queue-impl (elems comparator))

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

(defun make-queue (comparator)
  (make-queue-impl (vector) comparator))

(defun priority-queue? (self)
  (queue-impl? self))

(defun empty? (self)
  (= 0 (vector-length (queue-impl-elems self))))

(defun push (self item)
  (match self
    [(record queue-impl v c)
     (vector-append! v item)
     (reheap-up v c)]))

(defun pop (self)
  (match self
    [(record queue-impl v c)
     (if (empty? self) #nil
       (let [(top (vector-ref v 0)) (last (- (vector-length v) 1))]
       (vector-set! v 0 (vector-ref v last))
       (vector-remove! v last)
       (reheap-down v c)
       top))]))
