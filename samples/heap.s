
(defun heap:push (v x)
  (vector:append! v x)
  (let [(i (- (vector:length v) 1))]
    (while (> i 0)
      (let [(parent (quotient (- i 1) 2))]
        (if (< (vector:ref v i) (vector:ref v parent))
          (vector:swap! v i parent)
          (set! i parent))))))

(defun heap:pop (v)
  (let [(top (vector:ref v 0))
        (last (- (vector:length v) 1))]
    (vector:set! v 0 (vector:ref v last))
    (vector:remove! v last)
    (let [(root 0) (left 1) (right 2)]
      (while (< left last)
        (let [(child (cond [(not (< right last)) left]
                           [(< (vector:ref v left) (vector:ref v right)) left]
                           [else right]))]
          (if (< (vector:ref v child) (vector:ref v root))
            (do
              (vector:swap! v root child)
              (set! root child)
              (set! left (+ 1 (* 2 root)))
              (set! right (+ 2 (* 2 root))))
            (return top)))))
     top))

(def v (vector))

(heap:push v 3)
(heap:push v 2)
(heap:push v 1)
(heap:push v 4)
(heap:push v 5)

(print-line v)

(print-line (heap:pop v))
(print-line (heap:pop v))
(print-line (heap:pop v))
(print-line (heap:pop v))
(print-line (heap:pop v))
