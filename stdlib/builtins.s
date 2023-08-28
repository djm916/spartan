
; Spartan standard library of built-in procedures and macros

; This file is pre-loaded when the interpreter starts and
; itself loads several other files.

(namespace core
  (defun a () 42)
  (defun b () 24)
;  (defmacro unless (test & body)
;    `(if ,test
;      nil
;      (do ,@body)))
      
;  (defun memoize (proc) 
;    (let ((value-ready? false)
;          (cached-value nil))
;      (fun ()
;        (unless value-ready?
;          (set! cached-value (proc))
;          (set! value-ready? true))
;        cached-value)))
  
;  (defmacro delay (exp) `(memoize (fun () ,exp)))
  
;  (defun force (p) (p))
)

;(load "stdlib/test.s")
;(load "stdlib/lists.s")
;(load "stdlib/vectors.s")
;(load "stdlib/defstruct.s")
;(load "stdlib/streams.s")
