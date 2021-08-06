(defun sum (&xs)
  (letrec ((loop (fun (result xs)
                   (if (= xs ())
                     result
                     (loop (+ result (car xs)) (cdr xs))))))
    (loop 0 &xs)))

(defun sum (init &xs)
  (if (= &xs ())
    init
    (apply sum (cons (+ init (car &xs)) (cdr &xs)))))
