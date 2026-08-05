
(in-module spartan.base)

(export make-promise ; must export promise constructor as it's used in the macro expansion of "delay"
        delay
        force)

(defrecord promise (thunk value forced))

; Redefine promise constructor to supply default arguments
(let ((orig make-promise))
  (set! make-promise (fun (thunk) (orig thunk #nil #false))))

(defmacro delay (exp)
  `(spartan.base:make-promise (fun () ,exp)))

(defun force (p)
  (when (not (promise-forced p))
    (set-promise-value! p ((promise-thunk p)))
    (set-promise-forced! p #true)
    (set-promise-thunk! p #nil))
  (promise-value p))
