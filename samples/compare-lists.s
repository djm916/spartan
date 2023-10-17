
(defun compare (a b)
  (cond [(< a b) -1]
        [(> a b) +1]
        [else     0]))

(def x '(1 2 3))
(def y '(1 2))

(print-line (list-compare x y compare))
