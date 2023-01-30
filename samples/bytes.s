(def b (bytes/new 10))

; this only prints x once
; possible bug due to tail call elimination of the call to print-line?
;(defun f ()
;  (while true
;    (print-line "x")))

;(while (not (empty? b))
;  (print-line (bytes/pop! b)))
  
(f)
