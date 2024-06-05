
(in-package spartan.core)

(defrecord promise (thunk has-value value))

(defmacro delay (exp)
  `(make-promise (fun () ,exp) false void))

(defun force (p)
  (if (not (promise? p))
    p)
  (if (not (promise-has-value p))
    (do
      (promise-value-set! p ((promise-thunk p)))
      (promise-has-value-set! p true)))
  (promise-value p))
