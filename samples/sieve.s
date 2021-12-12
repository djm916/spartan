
(defun not-factor? (y) (fun (x) (not (= (% x y) 0))))

(defun prime-sieve (n)
  (defun loop (xs)
    (if (= () xs) ()
      (let* ((x (car xs))
             (xs (filter (not-factor? x) xs)))
        (cons x (loop xs)))))
  (loop (iota 2 n)))

(prime-sieve 10000)

