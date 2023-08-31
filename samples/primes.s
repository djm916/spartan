
(require "samples/prime-sieve.s")

; Define a stream of prime numbers using the prime generator defined above
(def primes (spartan.stream:stream (make-prime-generator)))

; Display some primes

(def N 10)
(print-line "The first " N " primes are:")
(spartan.stream:for-each print-line (spartan.stream:take N primes))
