
(load "stdlib/streams.s")

(defun not-factor? (y) (fun (x) (not (= (% x y) 0))))

(defun int-range (from to)
  (fun ()
    (if (> from to)
      nil
      (let ((next from))
        (set! from (+ 1 from))
        next))))

(defun prime-sieve (n)
  (defun prime-sieve (xs)
    (if (stream/empty? xs) ()
      (let* ((x (stream/car xs))
             (xs (stream/filter (not-factor? x) xs)))
        (cons x (prime-sieve xs)))))
  (prime-sieve (stream/new (int-range 2 n))))

(print-line (prime-sieve 100))
