(defun even (n)
  (if (= 0 n)
    true
    (odd (- n 1))))

(defun odd (n)
  (if (= 0 n)
    false
    (even (- n 1))))
