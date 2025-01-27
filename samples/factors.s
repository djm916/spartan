
; Computes the prime factors of a given number

; Usage: spartan factors.s N

(defun integer-stream (n)
  (stream-cons n (integer-stream (+ 1 n))))
  
(defun divisible? (x y) (= (remainder x y) 0))

(defun prime-sieve (s)
  (stream-cons (stream-car s)
               (prime-sieve (stream-filter (fun (n) (not (divisible? n (stream-car s))))
                                           (stream-cdr s)))))

(defun prime-factors (n)
  (def primes (prime-sieve (integer-stream 2)))
  (def factors ())
  (while (> n 1)
    (let ((p (stream-car primes)))
      (while (= 0 (remainder n p))
        (set! factors (adjoin p factors))
        (set! n (quotient n p)))
      (set! primes (stream-cdr primes))))
  factors)

(if (empty? *command-line-args*)
  (abort "required command line argument"))

(def N (string->int (first *command-line-args*)))

(print-line (prime-factors N))
