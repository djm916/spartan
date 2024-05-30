; Streams Library

; A stream is a lazyily-evaluated, possibly infinite, sequence with a list-like interface.

; A stream is essentially a procedure (called a "promise"), which generates a sequence of values, calculated on demand.
; Each time the promise is invoked, it returns a pair: the next value in the stream, and another promise to compute the remaining values.

; This implementation of streams relys on the "delay" macro and the
; "force" procedure. The user defines a "generator" procedure,
; which is called each time the stream is forced. It should
; return nil to indicate an end of stream condition. Otherwise, it
; should return the next value in the stream.

; A stream may be either finite or infinite. Be warned that some
; stream operations cannot be computed on infinite streams. 

; Create a new stream
;
; Parameters:
;
; gen   A generator procedure that produces succesive stream values
;       each time it is called.

(in-package spartan.core)

; Add an element to the front of a stream

(defmacro stream-cons (e s) `(delay (list ,e ,s)))

;(defmacro stream-cons (e s) `(cons ,e (delay ,s)))

; Return the first element of a stream

(defun stream-car (s) (car (force s)))

; Return the rest of a stream

(defun stream-cdr (s) (cadr (force s)))

; Determine if a stream is empty

(defun stream-empty? (s) (null? (force s)))

; The empty stream

(def stream-empty (delay ()))

(defun stream-map (f s)
  (if (stream-empty? s) s
    (stream-cons (f (stream-car s)) (stream-map f (stream-cdr s)))))

(defun stream-for-each (f s)
  (if (stream-empty? s) void
    (do (f (stream-car s))
        (stream-for-each f (stream-cdr s)))))

(defun stream-filter (f s)
  (cond ((stream-empty? s) s)
        ((f (stream-car s)) (stream-cons (stream-car s) (stream-filter f (stream-cdr s))))
        (else (stream-filter f (stream-cdr s)))))

(defun stream-take (n s)
  (if (or (stream-empty? s) (= n 0)) stream-empty
    (stream-cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

(defun stream-reduce (f i s)
  (if (stream-empty? s) i
    (stream-reduce f (f i (stream-car s)) (stream-cdr s))))

(defun stream->list (s)
  (if (stream-empty? s) ()
    (cons (stream-car s) (stream->list (stream-cdr s)))))

(defun stream-enumerate (i s)
  (if (stream-empty? s) ()
    (stream-cons (list i (stream-car s)) (stream-enumerate (+ i 1) (stream-cdr s)))))
