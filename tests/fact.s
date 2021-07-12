
(letrec ((fact (fun (n)
                 (if (= 0 n)
                   1
                   (* n (fact (- n 1)))))))
  (fact 6))
