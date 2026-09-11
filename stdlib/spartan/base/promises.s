
(in-module spartan.base)

(export promise ; must export promise constructor as it's used in the macro expansion of "delay"
        delay
        force)

(defrecord Promise
  promise
  promise?
  (thunk promise-thunk promise-set-thunk!)
  (value promise-value promise-set-value!)
  (forced promise-forced? promise-set-forced!))

; Redefine promise constructor to supply default arguments
(let ((orig promise))
  (set! promise (fun (thunk) (orig thunk #nil #false))))

(defmacro delay (exp)
  `(spartan.base:promise (fun () ,exp)))

(defun force (p)
  (when (not (promise-forced? p))
    (promise-set-value! p ((promise-thunk p)))
    (promise-set-forced! p #true)
    (promise-set-thunk! p #nil))
  (promise-value p))
