
(require "samples/prime-sieve.s")

; Define a stream of prime numbers using the prime generator defined above
(def primes (make-stream (make-prime-generator)))

; Display some primes

(def N 10)
(print-line "The first " N " primes are:")
(stream-for-each print-line (stream-take N primes))
