; Example of a variadic, recursive macro "any".
;
; "any" implements a short-circuiting "or" operation as
; a macro, transforming it into an equivalent series of
; nested "if"s.
; 
; (any) => false
;
; (any xs...) => (if (car xs) true (any (cdr xs)))
;

(defmacro any (& xs)
  (if (empty? xs)
      false
      `(if ,(car xs) true (any ,@(cdr xs)))))

