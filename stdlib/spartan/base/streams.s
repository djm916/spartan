; Streams library

(in-module spartan.base)

(export stream-eager
        stream-lazy
        stream-delay
        make-stream
        *empty-stream*
        stream?
        stream
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
        stream-length
        stream->list
        generator->stream
)

; Define stream type
; A stream is implemented as a mutable boxed promise. The promise is a mutable
; pair, where:
;   - the car is a tag, the symbol 'lazy or 'eager
;   - the cdr is a thunk in case tag is 'lazy or an evaluated value in case of 'eager
(defrecord <stream>
  make-stream
  stream?
  (promise stream-promise stream-set-promise!))

(defmacro stream-lazy (expr)
  `(spartan.base:make-stream (spartan.base:cons 'lazy (fun () ,expr))))

(defun stream-eager (expr)
  (make-stream (cons 'eager expr)))

(defmacro stream-delay (expr)
  `(spartan.base:stream-lazy (spartan.base:stream-eager ,expr)))

(defun stream-force (stream)
  (let ((promise (stream-promise stream)))
    (cond ((= (car promise) 'eager)
           (cdr promise))
          ((= (car promise) 'lazy)
           (let* ((stream* ((cdr promise)))
                  (promise  (stream-promise stream)))
             (if (not (= (car promise) 'eager))
               (do (set-car! promise (car (stream-promise stream*)))
                   (set-cdr! promise (cdr (stream-promise stream*)))
                   (stream-set-promise! stream* promise)))
             (stream-force stream))))))

(def *empty-stream* (stream-delay (cons #nil #nil)))

(defun stream-empty? (stream)
  (identical? (stream-force stream)
              (stream-force *empty-stream*)))

(defmacro stream-adjoin (obj stream)
  `(spartan.base:stream-eager (spartan.base:cons (stream-delay ,obj) (stream-lazy ,stream))))

(defun stream-first (stream)
  (stream-force (car (stream-force stream))))

(defun stream-rest (stream)
  (cdr (stream-force stream)))

(defmacro stream-fun (params :rest body)
  `(fun ,params (spartan.base:stream-lazy (do ,@body))))

(defmacro defstream (name params :rest body)
  `(def ,name (spartan.base:stream-fun params body)))

(defmacro stream (:rest elems)
  (if (empty? elems)
    '*empty-stream*
    `(spartan.base:stream-adjoin ,(first elems) (spartan.base:stream ,@(rest elems)))))

(defun __stream-length (n stream)
  (if (stream-empty? stream) n (__stream-length (+ 1 n) (stream-rest stream))))

(defun stream-length (stream)
  (__stream-length 0 stream))

(def __stream-take
  (stream-fun (n stream)
    (if (or (stream-empty? stream) (= 0 n))
      *empty-stream*
      (stream-adjoin (stream-first stream)
                     (__stream-take (- n 1) (stream-rest stream))))))

(defun stream-take (n stream)
  (__stream-take n stream))

(def __stream-drop
  (stream-fun (n stream)
    (if (or (= 0 n) (stream-empty? stream))
      stream
      (__stream-drop (- n 1) (stream-rest stream)))))

(defun stream-drop (n stream)
  (__stream-drop n stream))

(defun stream->list (stream)
  (if (stream-empty? stream)
    ()
    (adjoin (stream-first stream)
            (stream->list (stream-rest stream)))))

(def __stream-filter
  (stream-fun (pred stream)
    (cond [(stream-empty? stream)
           *empty-stream*]
          [(pred (stream-first stream))
           (stream-adjoin (stream-first stream)
                          (__stream-filter pred (stream-rest stream)))]
          [else
           (__stream-filter pred (stream-rest stream))])))

(defun stream-filter (pred stream)
  (__stream-filter pred stream))

(def __stream-map
  (stream-fun (f stream)
    (if (stream-empty? stream)
      *empty-stream*
      (stream-adjoin (f (stream-first stream))
                     (__stream-map f (stream-rest stream))))))

(defun stream-map (f stream)
  (__stream-map f stream))

(defun stream-reduce (f e stream)
  (if (stream-empty? stream)
    e
    (stream-reduce f (f e (stream-first stream)) (stream-rest stream))))

(defun stream-ref (n stream)
  (if (= 0 n)
    (stream-first stream)
    (stream-ref (- n 1) (stream-rest stream))))
