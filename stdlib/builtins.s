
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(load "stdlib/lists.s")
(load "stdlib/vectors.s")
(load "stdlib/streams.s")
(load "stdlib/defstruct.s")

(defun even? (x) (= 0 (% x 2)))
(defun odd?  (x) (not (even? x)))

(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (set! ,a ,b)
       (set! ,b ,tmp))))

(defun memoize (proc) 
  (let ((value-ready? false)
        (cached-value nil))
    (fun ()
      (if (not value-ready?)
          (do (set! cached-value (proc))
              (set! value-ready? true)))
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
