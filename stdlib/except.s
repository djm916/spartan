
(in-package spartan.core)

(def *winders* ())

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
  (set! *winders* (cons (list pre post) *winders*))
  (let ((result (thunk)))
    (set! *winders* (cdr *winders*))
    (post)
    result))

(set! call/cc
  (let ((oldcc call/cc))
    (fun (proc)
      (let ((save *winders*))
        (oldcc (fun (k)
                 (proc (fun (result)
                         (%do-winds *winders* save)
                         (k result)))))))))

(defun %do-winds (from to)
  (print-line "do winds")
  (set! *winders* from)
  (if (not (identical? from to))
      (cond ((null? from)
               (%do-winds from (cdr to))
               ((car (car to))))
            ((null? to)
               ((cadr (car from)))
               (%do-winds (cdr from) to))
            (else
               ((cadr (car from)))
               (%do-winds (cdr from) (cdr to))
               ((car (car to))))))
  (set! *winders* to))

(defrecord exception (name message))

(defun *default-exception-handler* (ex)
  (let ((message (string-concat "unhandled exception "
                                (symbol->string (exception-name ex))
                                ": "
                                (exception-message ex))))
    (abort message)))

;(def *top-level-continuation* void)
;(call/cc (fun (k) (set! *top-level-continuation* k)))

;(def *handlers* (list (list *default-exception-handler* *top-level-continuation*)))
;(def *handlers* (list (list *default-exception-handler* void)))
(def *handlers* ())

(defun push-handler (h k)
  (set! *handlers* (cons (list h k) *handlers*)))

(defun pop-handler ()
  (if (null? *handlers*)
    (do
      (print-line "handler stack empty")
      ())
    (do
      (let ((top (car *handlers*)))
        (set! *handlers* (cdr *handlers*))
        top))))

(defun try (thunk handler)
  (call/cc
    (fun (k)
      (dynamic-wind
        (fun () (print-line "in") (push-handler handler k))
        thunk
        (fun () (print-line "out") (pop-handler))))))

(defun raise (exn)
  (if (null? *handlers*)
    (*default-exception-handler* exn)
    (let* ((top (pop-handler))
           (h (car top))
           (k (cadr top)))
      (k (h exn)))))
