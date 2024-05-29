
(in-package spartan.core)

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
