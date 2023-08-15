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

(defun stream (gen)
  (delay
    (let ((next (gen)))
      (if (nil? next) ()
        (list next (stream gen))))))

; Return the next element of a stream

(defun stream:car (s) (car (force s)))

; Return the remainder of a stream

(defun stream:cdr (s) (cadr (force s)))

(defun stream:cons (e s) (delay (list e s)))

; Determine if a stream is empty

(defun stream:empty? (s) (empty? (force s)))

(defun stream:map (f s)
  (defun gen ()
    (if (stream:empty? s) nil
      (let ((next (f (stream:car s))))
        (set! s (stream:cdr s))
        next)))
  (stream gen))

(defun stream:for-each (f s)
  (cond ((stream:empty? s) nil)
        (else (f (stream:car s))
              (stream:for-each f (stream:cdr s)))))

(defun stream:filter (f s)
  (defun gen ()
    (if (stream:empty? s) nil
      (let ((next (stream:car s)))
        (set! s (stream:cdr s))
        (if (f next)
          next
          (gen)))))
  (stream gen))

(defun stream:take (n s)
  (defun gen ()
    (if (or (stream:empty? s) (= n 0)) nil
      (let ((next (stream:car s)))
        (set! s (stream:cdr s))
        (set! n (- n 1))
        next)))
  (stream gen))

(defun stream:reduce (f i s)
  (if (stream:empty? s) i
    (stream:reduce f (f i (stream:car s)) (stream:cdr s))))

(defun stream->list (s)
  (if (stream:empty? s) ()
    (cons (stream:car s)
          (stream->list (stream:cdr s)))))

(defun stream:enumerate (i s)
  (defun gen ()
    (if (stream:empty? s) nil
      (let ((next (list i (stream:car s))))
        (set! s (stream:cdr s))
        (set! i (+ 1 i))
        next)))
  (stream gen))
