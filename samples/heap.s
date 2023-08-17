
(defun heap:push (v x)
  (vector:append! v x)
  (let [(root (- (length v) 1))]
    (while (> root 0)
      (let [(parent (quotient (- root 1) 2))]
        (if (< (v root) (v parent))
          (vector:swap! v root parent)
          (set! root parent))))))

(defun heap:pop (v)
  (let [(top (v 0)) (last (- (length v) 1))]
    (set-at! v 0 (v last))
    (vector:remove! v last)
    (let [(root 0) (left 1) (right 2)]
      (while (< left last)
        (let [(child (cond [(not (< right last)) left]
                           [(< (v left) (v right)) left]
                           [else right]))]
          (if (< (v child) (v root))
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
