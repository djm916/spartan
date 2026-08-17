; Streams library

(in-module spartan.base)

(export stream-eager
        stream-lazy
        stream-delay
        make-stream
        stream?
        make-stream-promise
        make-stream-pair
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

(defrecord stream (val))

(defrecord stream-promise (tag val))

(defmacro stream-lazy (expr)
  `(make-stream (make-stream-promise 'lazy (fun () ,expr))))

(defun stream-eager (expr)
  (make-stream (make-stream-promise 'eager expr)))

(defmacro stream-delay (expr)
  `(stream-lazy (stream-eager ,expr)))

(defun stream-force (stream)
  (let ((promise (stream-val stream)))
    (cond ((= (stream-promise-tag promise) 'eager)
           (stream-promise-val promise))
          ((= (stream-promise-tag promise) 'lazy)
           (let* ((stream* ((stream-promise-val promise)))
                  (promise  (stream-val stream)))
             (if (not (= (stream-promise-tag promise) 'eager))
               (do (set-stream-promise-tag! promise (stream-promise-tag (stream-val stream*)))
                   (set-stream-promise-val! promise (stream-promise-val (stream-val stream*)))
                   (set-stream-val! stream* promise)))
             (stream-force stream))))))

(def *empty-stream* (stream-delay (make-stream-promise 'stream 'null)))

(defun stream-empty? (stream)
  (identical? (stream-force stream)
              (stream-force *empty-stream*)))

(defrecord stream-pair (fst rst))

(defmacro stream-adjoin (obj stream)
  `(stream-eager (make-stream-pair (stream-delay ,obj) (stream-lazy ,stream))))

(defun stream-first (stream)
  (stream-force (stream-pair-fst (stream-force stream))))

(defun stream-rest (stream)
  (stream-pair-rst (stream-force stream)))

(defmacro stream-fun (params & body)
  `(fun ,params (stream-lazy (do ,@body))))

(defmacro define-stream (name params & body)
  `(def ,name (stream-fun params body)))

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
