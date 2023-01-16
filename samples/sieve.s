
(defun not-factor? (y) (fun (x) (not (= (remainder x y) 0))))

(defun prime-sieve (n)
  (defun prime-sieve (xs)
    (if (empty? xs) ()
      (let* ((x (car xs))
             (xs (filter (not-factor? x) xs)))
        (cons x (prime-sieve xs)))))
  (prime-sieve (iota 2 n)))

(print-line "The prime numbers up to 100 are " (prime-sieve 100))
