; Example of a variadic, recursive macros "any?" and "all?"
; 
; They are implemented as a variadic, recursive macros which transform
; into a series of nested if-expressions.
;
; These are NOT intended to be a good example of writing these functions.
; Instead, they serve as a good test for the compiler's handling
; quasiquotation, unquote, unquote-splicing, and variadic and recursive
; macros.

; "any?" implements a short-circuiting "or" operation mapped over a list.
;
; (any?) => false
;
; (any? xs...) => (if (car xs) true (any? (cdr xs)))
;

(defmacro any? (& xs)
  (if (empty? xs)
    false
    `(if ,(car xs) true (any? ,@(cdr xs)))))

; "all?" implements a short-circuiting "and" operation mapped over a list.
;
; (all?) => true
;
; (all? xs...) => (if (not (car xs)) false (all? (cdr xs)))
;

(defmacro all? (& xs)
  (if (empty? xs)
    true
    `(if (not ,(car xs)) false (all? ,@(cdr xs)))))

