; Examples of stream usage

(load "stdlib/streams.txt")

;(defun generate-ints-up-to-3 (n)
;  (if (> n 3)
;    ()
;    (list n (+ n 1))))

;(def ints-up-to-3-stream (stream/new generate-ints-up-to-3 1))

;(let ((s ints-up-to-3-stream))
;  (while (not (stream/empty? s))
;    (print (stream/car s))
;    (set! s (stream/cdr s))))

(defun int-generator (n)
  (list n (+ n 1)))

(def s (stream/new int-generator 1))

(while (not (stream/empty? s))
  (print (stream/car s))
  (set! s (stream/cdr s)))
