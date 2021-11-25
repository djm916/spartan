
(load "stdlib/list.s")

(defmacro swap! (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (set! ,a ,b)
       (set! ,b ,tmp))))

(defun make-promise (proc) 
  (let ((value-ready? false)
        (cached-value nil))
    (fun ()
      (if (not value-ready?)
          (do (set! cached-value (proc))
              (set! value-ready? true)))
      cached-value)))

(defmacro delay (exp) `(make-promise (fun () ,exp)))

(defun force (promise) (promise))

; (compose f) => f
; (compose f g) => (fun (x) (g (f x)))
; (compose f g h) => (fun (x) (h (g (f x))))

(defmacro compose (&fs)
  (def x (gensym))
  (defun loop (fs)
    (if (empty? (cdr fs))
      `(,(car fs) ,x)
      `(,(car fs) ,(loop (cdr fs)))))
  `(fun (,x) ,(loop (reverse &fs))))

; (pipe x f) => (f x)
; (pipe x f g) => (g (f x))
; (pipe x f g h) => (h (g (f x)))

(defmacro pipe (x &fs)
  `(,(apply compose &fs) ,x))
