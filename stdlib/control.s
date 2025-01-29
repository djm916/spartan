
(in-package spartan.core)

; ============
; Dynamic Wind
; ============

; Together, dynamic-wind and call/cc manage a list of winders. A winder is a pair of in and out thunks established by a call to dynamic-wind.
; Whenever dynamic-wind is invoked, the in thunk is invoked, a new winder containing the in and out thunks is placed on the winders list,
; the body thunk is invoked, the winder is removed from the winders list, and the out thunk is invoked. This ordering ensures that the winder
; is on the winders list only when control has passed through in and not yet entered out. Whenever a continuation is obtained, the winders list
; is saved, and whenever the continuation is invoked, the saved winders list is reinstated. During reinstatement, the out thunk of each winder
; on the current winders list that is not also on the saved winders list is invoked, followed by the in thunk of each winder on the saved
; winders list that is not also on the current winders list. The winders list is updated incrementally, again to ensure that a winder is on the
; current winders list only if control has passed through its in thunk and not entered its out thunk.

(def *winders* ())

(defun dynamic-wind (pre thunk post)
  (pre)
  (set! *winders* (adjoin (list pre post) *winders*))
  (let ((result (thunk)))
    (set! *winders* (rest *winders*))
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
  (set! *winders* from)
  (if (not (identical? from to))
      (cond ((empty? from)
               (%do-winds from (rest to))
               ((first (first to))))
            ((empty? to)
               ((second (first from)))
               (%do-winds (rest from) to))
            (else
               ((second (first from)))
               (%do-winds (rest from) (rest to))
               ((first (first to))))))
  (set! *winders* to))

; ==========
; Exceptions
; ==========
;

(defrecord exception (name message))

(defun *default-exception-handler* (ex)
  (let ((message (string-concat "unhandled exception "
                                (symbol->string (exception-name ex))
                                ": "
                                (exception-message ex))))
    (error message)))

(def *handlers* ())

(defun %push-handler (h k)
  (set! *handlers* (adjoin (list h k) *handlers*)))

(defun %pop-handler ()
  (let ((top (first *handlers*)))
    (set! *handlers* (rest *handlers*))
    top))

(defun raise (exn)
  (if (empty? *handlers*)
    (*default-exception-handler* exn)
    (let* ((top (%pop-handler))
           (h (first top))
           (k (second top)))
      (k (h exn)))))

;(defun try (thunk handler)
;  (let ((save *handlers*))
;    (call/cc
;      (fun (k)
;        (dynamic-wind
;          (fun () (%push-handler handler k))
;          thunk
;          (fun () (set! *handlers* save)))))))

(defun with-exception-handler (handler thunk)
  (let ((save *handlers*))
    (call/cc
      (fun (k)
        (dynamic-wind
          (fun () (%push-handler handler k))
          thunk
          (fun () (set! *handlers* save)))))))

; <guard-exp> => (guard <guard-clause>* <exp>*)
; <guard-clause> => (<symbol> <exp>*)

(defmacro guard (clauses & body)
  (defun generate-handler-body (var clauses)
    (if (empty? clauses) ()
      (let ((clause (first clauses)))
        (adjoin `((= ',(first clause) (exception-name ,var)) ,@(rest clause))
                  (generate-handler-body var (rest clauses))))))
  (let ((var (gensym)))
    `(with-exception-handler
       (fun (,var)
         (cond ,@(generate-handler-body var clauses)))
       (fun () ,@body))))
