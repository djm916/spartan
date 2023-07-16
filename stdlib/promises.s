
(defun memoize (proc) 
  (let ((value-ready? false)
        (cached-value nil))
    (fun ()
      (unless value-ready?
        (set! cached-value (proc))
        (set! value-ready? true))
      cached-value)))

;(defstruct promise (proc))

;(defmacro delay (exp) `(promise (memoize (fun () ,exp))))

;(defun force (p) ((promise/proc p)))

(defmacro delay (exp) `(memoize (fun () ,exp)))

(defun force (p) (p))
