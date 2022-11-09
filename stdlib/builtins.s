
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(defun even? (x) (= 0 (remainder x 2)))

(defun odd?  (x) (not (even? x)))

(defmacro inc! (var)
  `(set! ,var (+ 1 ,var)))

(defmacro dec! (var)
  `(set! ,var (- ,var 1)))

(defmacro when (test & body)
  `(if ,test
     (do ,@body)
     nil))

(defmacro unless (test & body)
  `(if ,test
    nil
    (do ,@body)))

(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (set! ,a ,b)
       (set! ,b ,tmp))))

(defun memoize (proc) 
  (let ((value-ready? false)
        (cached-value nil))
    (fun ()
      (unless value-ready?
        (set! cached-value (proc))
        (set! value-ready? true))
      cached-value)))

(defmacro delay (exp) `(memoize (fun () ,exp)))

(defun force (promise) (promise))

; (compose f) => f
; (compose f g) => (fun (x) (g (f x)))
; (compose f g h) => (fun (x) (h (g (f x))))

(defmacro compose (& fs)
  (def x (gensym))
  (defun loop (fs)
    (if (empty? (cdr fs))
      `(,(car fs) ,x)
      `(,(car fs) ,(loop (cdr fs)))))
  `(fun (,x) ,(loop (reverse fs))))

; (pipe x f) => (f x)
; (pipe x f g) => (g (f x))
; (pipe x f g h) => (h (g (f x)))

(defmacro pipe (x & fs)
  `((compose ,@fs) ,x))

; (curry () ...) => (fun () ...)
; (curry (x) ...) => (fun (x) ...)
; (curry (x y) ...) => (fun (x) (fun (y) ...))

(defmacro curry (args & body)
  (defun loop (args)
    (cond ((empty? args)
           `(fun () ,@body))
          ((empty? (cdr args))
           `(fun (,(car args)) ,@body))
          (true
           `(fun (,(car args)) ,(loop (cdr args))))))
  (loop args))

(def require
  (let ((*files-loaded* ()))
    (fun (filename)
      (unless (contains? filename *files-loaded*)
        (set! *files-loaded* (cons filename *files-loaded*))
        (load filename)))))

(load "stdlib/lists.s")
(load "stdlib/vectors.s")
(load "stdlib/defstruct.s")
(load "stdlib/streams.s")
