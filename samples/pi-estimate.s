
(defun sum (term from to)
  (if (> from to)
    0
    (+ (term from) (sum term (+ 1 from) to))))

(def pi (* (sum (fun (k) (/ (exp -1.0 (* 1.0 k)) (+ (* 2 k) 1))) 0 10) 4.0))
(print-line pi)
