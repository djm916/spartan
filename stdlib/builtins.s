
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(set-current-package! (find-package 'spartan.core))

(defmacro in-package (package-name)
  `(let ((package (try-find-package ',package-name)))
     (if (nil? package)
       (set! package (make-package ',package-name)))
     (set-current-package! package)))

(defmacro inc! (var)
  `(set! ,var (+ 1 ,var)))

(defmacro dec! (var)
  `(set! ,var (- ,var 1)))

(defmacro when (test & body)
  `(if ,test
     (begin ,@body)
     nil))

(defmacro unless (test & body)
  `(if ,test
    nil
    (begin ,@body)))

(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (set! ,a ,b)
       (set! ,b ,tmp))))

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

(defmacro ->> (arg form & forms)
  (let ((result (append form arg)))
    (while (not (empty? forms))
      (set! result (append (car forms) result))
      (set! forms (cdr forms)))
    result))

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
    
(defun memoize (proc) 
  (let ((value-ready? false)
        (cached-value nil))
    (fun ()
      (unless value-ready?
        (set! cached-value (proc))
        (set! value-ready? true))
      cached-value)))

(defmacro delay (exp) `(memoize (fun () ,exp)))

(defun force (p) (p))

(defun min (x & xs)
  (let ((lo x))
    (while (not (empty? xs))
      (if (< (car xs) lo)
        (set! lo (car xs)))
      (set! xs (cdr xs)))
  lo))

(defun max (x & xs)
  (let ((hi x))
    (while (not (empty? xs))
      (if (> (car xs) hi)
        (set! hi (car xs)))
      (set! xs (cdr xs)))
  hi))

;(defmacro rec (symbol bindings & body)
;  (let ((vars (map bindings (fun (pair) (car pair))))
;        (inits (map bindings (fun (pair) (cadr pair)))))
;    `(letrec ((,symbol (fun vars ,@body)))
;       (apply ,symbol inits))))

(load "stdlib/lists.s")
(load "stdlib/vectors.s")
(load "stdlib/defstruct.s")
(load "stdlib/streams.s")
(load "stdlib/import.s")
(load "stdlib/except.s")
