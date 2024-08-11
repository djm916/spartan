
(in-package spartan.core)

(defrecord promise (thunk value has-value))

(defmacro delay (exp)
  `(make-promise (fun () ,exp) void false))

(defun force (p)
  (if (not (promise? p))
    p)
  (if (not (promise-has-value p))
    (do
      (promise-value-set! p ((promise-thunk p)))
      (promise-has-value-set! p true)
      (promise-thunk-set! p void)))
  (promise-value p))
