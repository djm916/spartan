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

(namespace 'spartan.stream)

(defun stream (gen)
  (delay
    (let ((next (gen)))
      (if (nil? next) ()
        (list next (stream gen))))))

; Return the first element of a stream

(defun car (s) (spartan.core:car (force s)))

; Return the rest of a stream

(defun cdr (s) (spartan.core:cadr (force s)))

; Add an element to the front of a stream

(defun cons (e s) (delay (list e s)))

; Determine if a stream is empty

(defun empty? (s) (spartan.core:empty? (force s)))

(defun map (f s)
  (defun gen ()
    (if (empty? s) nil
      (let ((next (f (car s))))
        (set! s (cdr s))
        next)))
  (stream gen))

(defun for-each (f s)
  (cond ((empty? s) nil)
        (else (f (car s))
              (for-each f (cdr s)))))

(defun filter (f s)
  (defun gen ()
    (if (empty? s) nil
      (let ((next (car s)))
        (set! s (cdr s))
        (if (f next)
          next
          (gen)))))
  (stream gen))

(defun take (n s)
  (defun gen ()
    (if (or (empty? s) (= n 0)) nil
      (let ((next (car s)))
        (set! s (cdr s))
        (set! n (- n 1))
        next)))
  (stream gen))

(defun reduce (f i s)
  (if (empty? s) i
    (reduce f (f i (car s)) (cdr s))))

(defun stream->list (s)
  (if (empty? s) ()
    (spartan.core:cons (car s) (stream->list (cdr s)))))

(defun enumerate (i s)
  (defun gen ()
    (if (empty? s) nil
      (let ((next (list i (car s))))
        (set! s (cdr s))
        (set! i (+ 1 i))
        next)))
  (stream gen))
