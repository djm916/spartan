
; Computes the prime factors of a given number

; Usage: spartan factors.s N

(require "stdlib/streams.s")

; Returns a function that determines if y is NOT a factor of its argument x
(defun not-factor? (y) (fun (x) (not (= (remainder x y) 0))))

; Generate the integers starting from a given value
(defun make-int-generator (start)
  (fun ()
    (let ((next start))
      (inc! start)
      next)))

; Generate the primes using the Sieve of Eratosthenes algorithm
(defun make-prime-generator ()
  ; Begin with the stream of all natural numbers from 2 (as 0 and 1 are not prime)
  (def nats (stream (make-int-generator 2)))
  (fun ()
    (let ((next-prime (stream:car nats))) ; The first value is a prime
      ; Filter out all the rest of the numbers that are a multiple of this prime
      (set! nats (stream:filter (not-factor? next-prime) nats))
      next-prime)))

(defun prime-factors (n)
  (def primes (stream (make-prime-generator)))
  (def factors ())
  (while (> n 1)
    (let ((p (stream:car primes)))
      (while (= 0 (remainder n p))
        (set! factors (cons p factors))
        (set! n (quotient n p)))
      (set! primes (stream:cdr primes))))
  factors)

(def N (string->int (car sys/args)))

(print-line (prime-factors N))
