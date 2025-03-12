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

(in-ns spartan.core)

; Add an element to the front of a stream

(defmacro stream-adjoin (e s) `(delay (list ,e ,s)))

;(defmacro stream-adjoin (e s) `(adjoin ,e (delay ,s)))

; Return the first element of a stream

(defun stream-first (s) (first (force s)))

; Return the rest of a stream

(defun stream-rest (s) (second (force s)))

; Determine if a stream is empty

(defun stream-empty? (s) (empty? (force s)))

; The empty stream

(def stream-empty (delay ()))

(defun stream-map (f s)
  (if (stream-empty? s) s
    (stream-adjoin (f (stream-first s)) (stream-map f (stream-rest s)))))

(defun stream-for-each (f s)
  (if (stream-empty? s) #nil
    (do (f (stream-first s))
        (stream-for-each f (stream-rest s)))))

(defun stream-filter (f s)
  (cond ((stream-empty? s) s)
        ((f (stream-first s)) (stream-adjoin (stream-first s) (stream-filter f (stream-rest s))))
        (else (stream-filter f (stream-rest s)))))

(defun stream-take (n s)
  (if (or (stream-empty? s) (= n 0)) stream-empty
    (stream-adjoin (stream-first s) (stream-take (- n 1) (stream-rest s)))))

(defun stream-drop (n s)
  (if (= 0 n) s (stream-drop (- n 1) (stream-rest s))))

(defun stream-reduce (f i s)
  (if (stream-empty? s) i
    (stream-reduce f (f i (stream-first s)) (stream-rest s))))

(defun stream-enumerate (i s)
  (if (stream-empty? s) ()
    (stream-adjoin (list i (stream-first s)) (stream-enumerate (+ i 1) (stream-rest s)))))

(defun stream-ref (i s)
  (stream-first (stream-drop i s)))

(defun stream->list (s)
  (if (stream-empty? s) ()
    (adjoin (stream-first s) (stream->list (stream-rest s)))))

(defun generator->stream (g)
  (delay
    (let ((result (g)))
      (if (nil? result)
        ()
        (list result (generator->stream g))))))
