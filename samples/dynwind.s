;; Setup a global list to record the order of execution
(def execution-log ())
(defun log-event! (message)
  (set! execution-log (concat execution-log (list message))))

;; Helper to reset the log between tests
(defun reset-log! ()
  (set! execution-log '()))

;; ====================================================================
;; TEST 1: Normal Execution Flow
;; ====================================================================
(print-line "TEST 1: Normal Execution Flow")
(reset-log!)

(dynamic-wind
  (fun () (log-event! 'before))
  (fun () (log-event! 'body) #nil)
  (fun () (log-event! 'after)))

;; Expected execution-log: '(before body after)
(print-line execution-log)

;; ====================================================================
;; TEST 2: Escaping the Body via Continuation (Non-local Exit)
;; ====================================================================
(print-line "TEST 2: Escaping the Body via Continuation (Non-local Exit)")
(reset-log!)

(def escape-cont #nil)

(call/cc (fun (cc) (set! escape-cont cc))) ; Capture an outside continuation

(dynamic-wind
  (fun () (log-event! 'before))
  (fun ()
    (log-event! 'body)
    (escape-cont #nil) ; Jump completely out of the block
    (log-event! 'body-unreachable))
  (fun () (log-event! 'after)))

;; Expected execution-log: '(before body after)
;; Note: 'after' runs even though the body did not finish normally.
(print-line execution-log)

;; ====================================================================
;; TEST 3: Re-entering the Body via Continuation
;; ====================================================================
(print-line "TEST 3: Re-entering the Body via Continuation")
(reset-log!)

(def reenter-cont #nil)

(dynamic-wind
  (fun () (log-event! 'enter-before))
  (fun ()
    (log-event! 'enter-body)
    ;; Capture the state right here inside the body
    (call/cc (fun (cc) (set! reenter-cont cc)))
    (log-event! 'inside-body-checkpoint))
  (fun () (log-event! 'enter-after)))

;; First pass log: '(enter-before enter-body inside-body-checkpoint enter-after)
(print-line execution-log)

;; Now, invoke the saved continuation to jump back INSIDE the body
(reenter-cont #true)

;; Expected final execution-log after re-entry:
;; '(enter-before enter-body inside-body-checkpoint enter-after
;;   enter-before inside-body-checkpoint enter-after)
;; Note: Re-entry forces 'enter-before' to run again, and exiting 
;; the body a second time forces 'enter-after' to run again.
(print-line execution-log)

;; ====================================================================
;; TEST 3: Re-entering Nested Body via Continuation
;; ====================================================================
(print-line "TEST 4: Re-entering Nested Body via Continuation")
(reset-log!)

(def inner-continuation #nil)

(dynamic-wind
  (fun () (log-event! 'outer-before))
  (fun ()
    (log-event! 'outer-body)
    ;; Nest a second dynamic-wind inside the first
    (dynamic-wind
      (fun () (log-event! 'inner-before))
      (fun ()
        (log-event! 'inner-body)
        ;; Capture inner continuation context
        (call/cc (fun (cc) (set! inner-continuation cc))))
      (fun () (log-event! 'inner-after))))
  (fun () (log-event! 'outer-after)))

;; --------------------------------------------------------------------
;; Initial Pass Results
;; --------------------------------------------------------------------
;; After the initial code execution finishes, the nested-log is:
;; '(outer-before outer-body inner-before inner-body inner-after outer-after)
(print-line execution-log)

;; --------------------------------------------------------------------
;; TRIGGERING THE JUMP
;; --------------------------------------------------------------------
;; We invoke the inner continuation from outside both blocks.
;; Scheme must re-enter BOTH the outer and inner contexts in order.
(log-event! '---trigger-jump---)
(inner-continuation #true)

;; --------------------------------------------------------------------
;; Expected Final Execution Log
;; --------------------------------------------------------------------
;; '(
;;   outer-before outer-body inner-before inner-body inner-after outer-after
;;   ---trigger-jump---
;;   outer-before inner-before inner-after outer-after
;;  )
;;
;; Notice the exact execution sequence after the jump:
;; 1. 'outer-before' runs first to enter the outer wind context.
;; 2. 'inner-before' runs next to enter the nested inner wind context.
;; 3. The inner body finishes (evaluating the rest of the inner-body block).
;; 4. 'inner-after' runs as control leaves the inner context.
;; 5. 'outer-after' runs as control leaves the outer context.
(print-line execution-log)

;; ====================================================================
;; TEST 5: Escaping Nested Windings via Continuation (Non-local Exit)
;; ====================================================================
(print-line "TEST 5: Escaping Nested Windings via Continuation (Non-local Exit)")
(reset-log!)

(def escape-cont #nil)

(call/cc (fun (cc) (set! escape-cont cc))) ; Save outer continuation

(dynamic-wind
  (fun () (log-event! 'outer-before))
  (fun ()
    (log-event! 'outer-body)
    (dynamic-wind
      (fun () (log-event! 'inner-before))
      (fun () (log-event! 'inner-body) (escape-cont #true))
      (fun () (log-event! 'inner-after))))
  (fun () (log-event! 'outer-after)))

;; Expected execution-log: '(outer-before outer-body inner-before inner-body inner-after outer-after)
;; Note: 'after' runs even though the body did not finish normally.
(print-line execution-log)
