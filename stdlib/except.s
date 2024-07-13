
(in-package spartan.core)

(def *winds* ())

; Together, dynamic-wind and call/cc manage a list of winders. A winder is a pair of in and out thunks established by a call to dynamic-wind.
; Whenever dynamic-wind is invoked, the in thunk is invoked, a new winder containing the in and out thunks is placed on the winders list,
; the body thunk is invoked, the winder is removed from the winders list, and the out thunk is invoked. This ordering ensures that the winder
; is on the winders list only when control has passed through in and not yet entered out. Whenever a continuation is obtained, the winders list
; is saved, and whenever the continuation is invoked, the saved winders list is reinstated. During reinstatement, the out thunk of each winder
; on the current winders list that is not also on the saved winders list is invoked, followed by the in thunk of each winder on the saved
; winders list that is not also on the current winders list. The winders list is updated incrementally, again to ensure that a winder is on the
; current winders list only if control has passed through its in thunk and not entered its out thunk.

(defun dynamic-wind (pre thunk post)
  (pre)
  (set! *winds* (cons (list pre post) *winds*))
  (let ((result (thunk)))
    (set! *winds* (cdr *winds*))
    (post)
    result))

(set! call/cc
  (let ((oldcc call/cc))
    (fun (proc)
      (let ((winds *winds*))
        (oldcc (fun (k)
                 (proc (fun (result)
                         (%do-winds *winds* winds)
                         (k result)))))))))

(defun %do-winds (from to)
  (set! *winds* from)
  (if (not (eq? from to))
      (cond ((null? from)
               (%do-winds from (cdr to))
               ((car (car to))))
            ((null? to)
               ((cdr (car from)))
               (%do-winds (cdr from) to))
            (else
               ((cdr (car from)))
               (%do-winds (cdr from) (cdr to))
               ((car (car to))))))
  (set! *winds* to))

(defrecord exception (name message))

(defun *default-handler* (ex)
  (let ((message (string-concat "unhandled exception "
                                (symbol->string (exception-name ex))
                                ": "
                                (exception-message ex))))
    (error message)))
  ;(print-line "unhandled exception " ex " was raised.")
  ;(print-traceback))

;(def *top-level-continuation* nil)
;(call/cc (fun (k) (set! *top-level-continuation* k)))

;(def *handlers* (list (list *default-handler* *top-level-continuation*)))

(def *handlers* (list (list *default-handler* void)))

(defun push-handler (h k)
  (set! *handlers* (cons (list h k) *handlers*)))

(defun pop-handler ()
  (let ((top (car *handlers*)))
    (set! *handlers* (cdr *handlers*))
    top))

(defun try (thunk handler)
  (call/cc
    (fun (k)
      (dynamic-wind
        (fun () (push-handler handler k))
        thunk
        (fun () (pop-handler))))))

(defun raise (exp)
  (let* ((top (pop-handler))
         (h (car top))
         (k (cadr top)))
    (k (h exp))))
