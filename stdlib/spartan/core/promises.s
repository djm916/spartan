
(in-ns spartan.core)

(defrecord promise (thunk value has-value))

(defmacro delay (exp)
  `(make-promise (fun () ,exp) #nil #false))

(defun force (p)
  (cond [(not (promise? p))
         p]
        [else
         (if (not (promise-has-value p))
           (do
             (set-promise-value! p ((promise-thunk p)))
             (set-promise-has-value! p #true)
             (set-promise-thunk! p #nil)))
         (promise-value p)]))
  