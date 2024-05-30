
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(set! spartan.core:*package* (find-package 'spartan.core))

;(defmacro in-package (package-name)
;  `(let ((package (try-find-package ',package-name)))
;     (if (nil? package)
;       (set! package (make-package ',package-name)))
;     (set! spartan.core:*package* package)))

(defmacro in-package (package-name)
  `(set! spartan.core:*package*
         (if (package-exists? ',package-name)
             (find-package ',package-name)
             (make-package ',package-name))))

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

; (compose f) => f
; (compose f g) => (fun (x) (g (f x)))
; (compose f g h) => (fun (x) (h (g (f x))))

(defmacro compose (& fs)
  (def x (gensym))
  (defun loop (fs)
    (if (null? (cdr fs))
      `(,(car fs) ,x)
      `(,(car fs) ,(loop (cdr fs)))))
  `(fun (,x) ,(loop (list-reverse fs))))


(defmacro ->> (arg form & forms)
  (let ((result (list-append form arg)))
    (list-fold-right list-append result forms)))

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
(load "stdlib/defrecord.s")
(load "stdlib/promises.s")
(load "stdlib/streams.s")
(load "stdlib/import.s")
(load "stdlib/except.s")
