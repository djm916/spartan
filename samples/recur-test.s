
(defun f (n)
  (defun loop (i)
    (if (= i n)
      'done
      (loop (+ i 1))))
  (loop 0))

(defun g (n)
  (rep ((i 0 (+ i 1)))
    (when (= i n) 'done)))

;(f 2000000)
(g 5000000)
