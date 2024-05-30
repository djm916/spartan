
(in-package spartan.core)

(defstruct promise (thunk has-value? value))

(defmacro delay (exp)
  `(make-promise (fun () ,exp) false nil))

(defun force (p)
  (if (not (promise? p))
    p)
  (if (not (p 'has-value?))
    (do
      (set! (p 'value) ((p 'thunk)))
      (set! (p 'has-value?) true)))
  (p 'value))
