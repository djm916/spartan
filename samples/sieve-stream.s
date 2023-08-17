
; Demonstration of the Sieve of Eratosthenes using streams.
;
; We define an infinite stream of prime numbers, and display the first N prime numbers.

(require "stdlib/streams.s")

; Returns a function that determines if y is NOT a factor of its argument x
(defun not-factor? (y) (fun (x) (not (= (remainder x y) 0))))

; Creates a generator that returns sucssive integers, starting from a given value
(defun make-int-generator (start)
  (fun ()
    (let ((next start))
      (inc! start)
      next)))

; Creates a prime number generator using the Sieve of Eratosthenes algorithm
(defun make-prime-generator ()
  ; Begin with the stream of all natural numbers from 2 (0 and 1 are not prime)
  (def nats (stream (make-int-generator 2)))
  (fun ()
    (let ((next-prime (stream:car nats))) ; The first value in the stream is prime
      ; Filter out numbers that are a multiple of this prime
      (set! nats (stream:filter (not-factor? next-prime) nats))
      next-prime)))

; Define a stream of prime numbers using the prime generator defined above
(def primes (stream (make-prime-generator)))

; Display some primes

(def N 10)
(print-line "The first " N " primes are:")
(stream:for-each print-line (stream:take N primes))
