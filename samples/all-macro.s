; Example of a variadic, recursive macro "all".
;
; "all" implements a short-circuiting "and" operation by transforming
; to an equivalent series of nested "if" forms.
; 
; (all) => false
;
; (all xs...) => (if (car xs) true (all (cdr xs)))
;

(defmacro all (&xs)
  (if (empty? &xs)
      'false
      `(if ,(car &xs) 'true (all ,@(cdr &xs)))))
