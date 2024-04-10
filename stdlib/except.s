
(in-package spartan.core)

(defstruct exception (name message))

(defun *default-handler* (ex)
  (let ((message (string-concat "unhandled exception " (symbol->string (ex 'name)) ": " (ex 'message))))
    (error message)))
  ;(print-line "unhandled exception " ex " was raised.")
  ;(print-traceback))

;(def *top-level-continuation* nil)
;(call/cc (fun (k) (set! *top-level-continuation* k)))

;(def *handlers* (list (list *default-handler* *top-level-continuation*)))

(def *handlers* (list (list *default-handler* nil)))

(defun push-handler (h k)
  (set! *handlers* (cons (list h k) *handlers*)))

(defun pop-handler ()
  (let ((top (car *handlers*)))
    (set! *handlers* (cdr *handlers*))
    top))

(defun try (thunk handler)
  (call/cc
    (fun (k)
      (push-handler handler k)
      (thunk))))

(defun raise (exp)
  (let* ((top (pop-handler))
         (h (car top))
         (k (cadr top)))
    (k (h exp))))
