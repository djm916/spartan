; Simple example of defining records

(defrecord point (x y))
(defun point->string (p)
  (string-concat "(" (format-decimal (point-x p)) ", " (format-decimal (point-y p)) ")"))
(def p (make-point 0.0 0.0))
(print-line "p is " p)
(print-line "p is a point? " (point? p))
(print-line "(type p) = " (type p))
(print-line "p = " (point->string p))
(point-x-set! p 1.0)
(point-y-set! p 2.0)
(print-line "p = " (point->string p))

; Experiment with record destructuring

; (record-destructor 'point) returns a procedure of 1 argument, a record of
; type point, and returns the values of its fields as a list, in the order
; declared in the definition of the point record
;(def unpack-point (record-destructor 'point))

;(defmacro with-point (exp & body)
;  `(apply (fun (x y) ,@body) (unpack-point ,exp)))

;(with-point p
;  (print-line "in with-point, p = (" x ", " y ")"))

;(defmacro with-point (exp vars & body)
;  `(apply (fun ,vars ,@body) (unpack-point ,exp)))

(with-point (make-point 1.0 2.0) (a b)
  (print-line "in with-point")
  (print-line "p = (" a ", " b ")"))
