(defun map (f xs)
  (if (= xs ())
    ()
  (cons (f (car xs)) (map f (cdr xs)))))
    
(map (fun (n) (* 2 n)) '(1 2 3))
