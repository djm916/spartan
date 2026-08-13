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

(in-module spartan.base)

(export make-stream-pair
        *empty-stream*
        stream-adjoin
        stream-first
        stream-rest
        stream-empty?
        stream-map
        stream-filter
        stream-foreach
        stream-take
        stream-drop
        stream-reduce
        stream-enumerate
        stream-ref
        stream->list
        generator->stream
)

(defrecord stream-pair (first rest))

; The unique empty stream object

(def *empty-stream* (delay (make-stream-pair #nil #nil)))

; Add an element to the front of a stream

(defmacro stream-adjoin (e s)
  `(spartan.base:delay (make-stream-pair ,e ,s)))

;(defmacro stream-adjoin (e s) `(spartan.base:adjoin ,e (spartan.base:delay ,s)))

; Return the first element of a stream

(defun stream-first (s) (stream-pair-first (force s)))

; Return the rest of a stream

(defun stream-rest (s) (stream-pair-rest (force s)))

; Determine if a stream is empty

(defun stream-empty? (s) (identical? s *empty-stream*))

(defun stream-map (f s)
  (if (stream-empty? s) s
    (stream-adjoin (f (stream-first s)) (stream-map f (stream-rest s)))))

(defun stream-foreach (f s)
  (if (not (stream-empty? s))
    (do (f (stream-first s))
        (stream-foreach f (stream-rest s)))))

(defun stream-filter (f s)
  (cond ((stream-empty? s) s)
        ((f (stream-first s)) (stream-adjoin (stream-first s) (stream-filter f (stream-rest s))))
        (else (stream-filter f (stream-rest s)))))

(defun stream-take (n s)
  (if (or (stream-empty? s) (= n 0)) *empty-stream*
    (stream-adjoin (stream-first s) (stream-take (- n 1) (stream-rest s)))))

(defun stream-drop (n s)
  (if (or (stream-empty? s) (= n 0)) s (stream-drop (- n 1) (stream-rest s))))

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
        *empty-stream*
        (make-stream-pair result (generator->stream g))))))
