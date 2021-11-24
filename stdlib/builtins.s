
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
