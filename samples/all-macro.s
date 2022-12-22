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

(print-line "(any?) = " (any?))
(print-line "(any? false) = " (any? false))
(print-line "(any? true) = " (any? true))
(print-line "(any? false true) = " (any? false true))
(print-line "(any? true false) = " (any? true false))
(print-line "(any? false false) = " (any? false false))
(print-line "(any? true true) = " (any? true true))

(print-line "(all?) = " (all?))
(print-line "(all? false) = " (all? false))
(print-line "(all? true) = " (all? true))
(print-line "(all? false true) = " (all? false true))
(print-line "(all? true false) = " (all? true false))
(print-line "(all? true true) = " (all? true true))
(print-line "(all? false false) = " (all? false false))

