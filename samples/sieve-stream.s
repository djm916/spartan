
(require "stdlib/streams.s")

(defun not-factor? (y) (fun (x) (not (= (remainder x y) 0))))

(defun make-int-generator (start)
  (fun ()
    (let ((next start))
      (set! start (+ 1 start))
      next)))

(defun make-prime-generator ()
  (def nats (stream/new (make-int-generator 2)))
  (fun ()
    (let ((next-prime (stream/car nats)))
      (set! nats (stream/filter (not-factor? next-prime) nats))
      next-prime)))

(def primes (stream/new (make-prime-generator)))

(print-line (stream->list (stream/take 100 primes)))
