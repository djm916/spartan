; Streams library

; A "stream" is a lazyily-evaluated, possibly infinite, sequence.

; In this implementation, a stream is a promise that, when forced, returns a
; pair: the stream's first element, and the rest of the stream (another promise).

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

(defun stream-car (s) (first (force s)))

; Return the rest of a stream

(defun stream-cdr (s) (second (force s)))

; Determine if a stream is empty

(defun stream-empty? (s) (empty? (force s)))

; The empty stream

(def stream-empty (delay ()))

(defun stream-map (f s)
  (if (stream-empty? s) s
    (stream-cons (f (stream-car s)) (stream-map f (stream-cdr s)))))

(defun stream-for-each (f s)
  (if (stream-empty? s) #nil
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

(defun stream-enumerate (i s)
  (if (stream-empty? s) ()
    (stream-cons (list i (stream-car s)) (stream-enumerate (+ i 1) (stream-cdr s)))))

(defun stream->list (s)
  (if (stream-empty? s) ()
    (adjoin (stream-car s) (stream->list (stream-cdr s)))))

(defun generator->stream (g)
  (delay
    (let ((result (g)))
      (if (nil? result)
        ()
        (list result (generator->stream g))))))
