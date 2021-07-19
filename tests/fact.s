
(letrec ((fact (fun (n)
                 (if (= 0 n)
                   1
                   (* n (fact (- n 1)))))))
  (fact 6))

(defun fact (n)
  (letrec ((loop (fun (n p)
                   (if (= 0 n)
                     p
                     (loop (- n 1) (* n p))))))
    (loop n 1)))
