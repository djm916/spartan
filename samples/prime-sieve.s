
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
  (def nats (spartan.stream:stream (make-int-generator 2)))
  (fun ()
    (let ((next-prime (spartan.stream:car nats))) ; The first value in the stream is prime
      ; Filter out numbers that are a multiple of this prime
      (set! nats (spartan.stream:filter (not-factor? next-prime) nats))
      next-prime)))
