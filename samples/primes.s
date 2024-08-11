(defun divisible? (x y)
  (= (remainder x y) 0))

; Define a stream of integers
(defun integer-stream (n)
  (stream-cons n (integer-stream (+ 1 n))))

(defun prime-sieve (s)
  (stream-cons (stream-car s)
               (prime-sieve (stream-filter (fun (n) (not (divisible? n (stream-car s))))
                                           (stream-cdr s)))))

; Define a stream of prime numbers using the prime generator defined above
(def primes (prime-sieve (integer-stream 2)))

; Display some primes
(def N 20)
(print-line "The first " N " primes are:")
(stream-for-each print-line (stream-take N primes))
