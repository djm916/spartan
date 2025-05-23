
(defun not-factor? (y) (fun (x) (not (= (remainder x y) 0))))

(defun range (lo hi)
  (reverse
    (rec loop ((n lo) (result ()))
      (if (< hi n) result
        (loop (+ 1 n) (adjoin n result))))))

(defun prime-sieve (n)
  (defun prime-sieve (xs)
    (if (empty? xs) ()
      (let* ((x (first xs))
             (xs (filter (not-factor? x) xs)))
        (adjoin x (prime-sieve xs)))))
  (prime-sieve (range 2 n)))

(print-line "The prime numbers up to 100 are " (prime-sieve 100))
