
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(set-current-module! (the-module 'spartan.base))

(def %quasiquote #nil)
(defun %quasiquote (exp level)
  (defun unquote? (form)
    (and (list? form) (not (empty? form)) (symbol? (first form)) (= (first form) 'unquote)))
  (defun unquote-splicing? (form)
    (and (list? form) (not (empty? form)) (symbol? (first form)) (= (first form) 'unquote-splicing)))
  (defun quasiquote? (form)
    (and (list? form) (not (empty? form)) (symbol? (first form)) (= (first form) 'quasiquote)))
  (if (< level 0)
    (error "unquote or unquote-splicing form must appear within quasiquote"))
  (cond [(empty? exp) ()]
        [(not (list? exp)) (list 'quote exp)]
        [else
          (let [(subexp (first exp))]
            (cond [(unquote? subexp)
                     (if (= level 0)
                       (list 'spartan.base:adjoin (second subexp) (%quasiquote (rest exp) 0))
                       (list 'spartan.base:adjoin (%quasiquote subexp (- level 1)) (%quasiquote (rest exp) level)))]
                  [(unquote-splicing? subexp)
                     (if (= level 0)
                       (list 'spartan.base:concat (second subexp) (%quasiquote (rest exp) 0))
                       (list 'spartan.base:adjoin (%quasiquote subexp (- level 1)) (%quasiquote (rest exp) level)))]
                  [(quasiquote? subexp)
                     (list 'spartan.base:adjoin (%quasiquote subexp (+ 1 level)) (%quasiquote (rest exp) level))]
                  [else
                     (list 'spartan.base:adjoin (%quasiquote (first exp) level) (%quasiquote (rest exp) level))]))]))

; quasiquote implemented as a macro; just calls the procedure %quasiquote on its (unevaluated) argument
;(defmacro quasiquote (exp)
;  (%quasiquote exp 0))

(defmacro in-module (module-name)
  `(spartan.base:set-current-module!
     (let ((m (spartan.base:find-module ',module-name)))
       (if (not (nil? m)) m (spartan.base:make-module ',module-name)))))

(defmacro export (& symbols)
  `(spartan.base:module-export ',symbols))

(export ->> compose curry in-module export inc! dec! let-values max min rec defrec
        swap! when unless import)

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
    (if (empty? (rest fs))
      `(,(first fs) ,x)
      `(,(first fs) ,(loop (rest fs)))))
  `(fun (,x) ,(loop (reverse fs))))

; (->> x (f ...)) => (f ... x)
; (->> x (f ...) (g ...)) => (g ... (f ... x))
; (->> x (f ...) (g ...) (h ...)) => (h ... (g ... (f ... x)))

(defmacro ->> (arg form & forms)
  (for ((forms forms (rest forms))
        (result (append arg form) (append result (first forms))))
    ((empty? forms) result)))

; (curry () ...) => (fun () ...)
; (curry (x) ...) => (fun (x) ...)
; (curry (x y) ...) => (fun (x) (fun (y) ...))

(defmacro curry (args & body)
  (defun loop (args)
    (cond ((empty? args)
           `(fun () ,@body))
          ((empty? (rest args))
           `(fun (,(first args)) ,@body))
          (#true
           `(fun (,(first args)) ,(loop (rest args))))))
  (loop args))
    
(defun min (x & xs)
  (let ((lo x))
    (while (not (empty? xs))
      (if (< (first xs) lo)
        (set! lo (first xs)))
      (set! xs (rest xs)))
  lo))

(defun max (x & xs)
  (let ((hi x))
    (while (not (empty? xs))
      (if (> (first xs) hi)
        (set! hi (first xs)))
      (set! xs (rest xs)))
  hi))

(load "spartan/base/lists.s")

; (rec f ((var1 init1) ... (varN initN)) body...)
; ==>
; (letrec ((f (fun (var1 ... varN) body..)))
;   (f init1 ... initN))
(defmacro rec (symbol bindings & body)
  (let ((vars (map first bindings))
        (inits (map second bindings)))
    `(letrec ((,symbol (fun ,vars ,@body)))
       (,symbol ,@inits))))

; (defrec
;   (f (fun (param ...) body...))
;   ...)
; ==>
; (def f #nil)
; (set! f (fun (param ...) body...))

(defmacro defrec (& forms)
  `(do
     ,@(map (fun (form) `(def ,(first form) #nil)) forms)
     ,@(map (fun (form) `(set! ,(first form) ,(second form))) forms)))

; (let-values (((var...) init)...) body...)
; ==>
; (apply (fun (var...) (do body...)) init)
;

(defmacro let-values (bindings & body)
  (rec loop ((bindings bindings))
    (if (empty? bindings)
      `(do ,@body)
      (let* [(binding (first bindings))
             (formals (first binding))
             (exp (second binding))]
        `(apply (fun ,formals ,(loop (rest bindings))) ,exp)))))

(load "spartan/base/vectors.s")
(load "spartan/base/defrecord.s")
(load "spartan/base/promises.s")
(load "spartan/base/streams.s")
(load "spartan/base/import.s")
(load "spartan/base/control.s")
(load "spartan/base/ports.s")
