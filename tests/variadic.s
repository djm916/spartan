(defun sum (&xs)
  (letrec ((loop (fun (result xs)
                   (if (= xs ())
                     result
                     (loop (+ result (first xs)) (rest xs))))))
    (loop 0 &xs)))
