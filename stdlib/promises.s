
(in-package spartan.core)

(defrecord promise (thunk value has-value))

(defmacro delay (exp)
  `(make-promise (fun () ,exp) #nil #false))

(defun force (p)
  (cond [(not (promise? p))
         (print-line "forced non-promise")
         p]
        [else
         (if (not (promise-has-value p))
           (do
             (promise-value-set! p ((promise-thunk p)))
             (promise-has-value-set! p #true)
             (promise-thunk-set! p #nil)))
         (promise-value p)]))
  