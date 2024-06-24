
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(set! spartan.core:*package* (find-package 'spartan.core))

(defun %quasiquote (exp level)
  (defun unquote? (form)
    (and (list? form) (not (null? form)) (symbol? (car form)) (= (car form) 'unquote)))
  (defun unquote-splicing? (form)
    (and (list? form) (not (null? form)) (symbol? (car form)) (= (car form) 'unquote-splicing)))
  (defun quasiquote? (form)
    (and (list? form) (not (null? form)) (symbol? (car form)) (= (car form) 'quasiquote)))
  (if (< level 0)
    (error "unquote or unquote-splicing form must appear within quasiquote"))
  (cond [(null? exp) ()]
        [(not (list? exp)) (list 'quote exp)]
        [else
          (let [(subexp (car exp))]
            (cond [(unquote? subexp)
                     (if (= level 0)
                       (list 'spartan.core:cons (cadr subexp) (%quasiquote (cdr exp) 0))
                       (list 'spartan.core:cons (%quasiquote subexp (- level 1)) (%quasiquote (cdr exp) level)))]
                  [(unquote-splicing? subexp)
                     (if (= level 0)
                       (list 'spartan.core:list-concat (cadr subexp) (%quasiquote (cdr exp) 0))
                       (list 'spartan.core:cons (%quasiquote subexp (- level 1)) (%quasiquote (cdr exp) level)))]
                  [(quasiquote? subexp)
                     (list 'spartan.core:cons (%quasiquote subexp (+ 1 level)) (%quasiquote (cdr exp) level))]
                  [else
                     (list 'spartan.core:cons (%quasiquote (car exp) level) (%quasiquote (cdr exp) level))]))]))


; quasiquote macro
; NOTE: turned off; using builtin quasiquote special form!
(defmacro quasiquote (exp)
  (%quasiquote exp 0))

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
     (do ,@body)))

(defmacro unless (test & body)
  `(if (not (,test))
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

; (->> x (f ...)) => (f ... x)
; (->> x (f ...) (g ...)) => (g ... (f ... x))
; (->> x (f ...) (g ...) (h ...)) => (h ... (g ... (f ... x)))

(defmacro ->> (arg form & forms)
  (for ((forms forms (cdr forms))
        (result (list-append form arg) (list-append (car forms) result)))
    ((null? forms) result)))

;  (rec loop [(result (list-append form arg))
;             (forms forms)]
;    (if (null? forms)
;      result
;      (loop (list-append (car forms) result) (cdr forms)))))

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
    (while (not (null? xs))
      (if (< (car xs) lo)
        (set! lo (car xs)))
      (set! xs (cdr xs)))
  lo))

(defun max (x & xs)
  (let ((hi x))
    (while (not (null? xs))
      (if (> (car xs) hi)
        (set! hi (car xs)))
      (set! xs (cdr xs)))
  hi))

(load "stdlib/lists.s")

; (rec f ((var1 init1) ... (varN initN)) body...)
; ==>
; (letrec ((f (fun (var1 ... varN) body..)))
;   (f init1 ... initN))
(defmacro rec (symbol bindings & body)
  (let ((vars (list-map car bindings))
        (inits (list-map cadr bindings)))
    `(letrec ((,symbol (fun ,vars ,@body)))
       (,symbol ,@inits))))

(defmacro let-values (bindings & body)
  (rec loop ((bindings bindings))
    (if (null? bindings)
      `(do ,@body)
      (let* [(binding (car bindings))
             (formals (car binding))
             (exp (cadr binding))]
        `(apply (fun ,formals ,(loop (cdr bindings))) ,exp)))))

(load "stdlib/vectors.s")
(load "stdlib/defrecord.s")
(load "stdlib/promises.s")
(load "stdlib/streams.s")
(load "stdlib/import.s")
(load "stdlib/except.s")
