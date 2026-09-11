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
; (any?) => #false
;
; (any? xs...) => (if (first xs) #true (any? (rest xs)))
;

(defmacro any? (:rest xs)
  (if (empty? xs)
    #false
    `(if ,(first xs) #true (any? ,@(rest xs)))))

; "all?" implements a short-circuiting "and" operation mapped over a list.
;
; (all?) => #true
;
; (all? xs...) => (if (not (first xs)) #false (all? (rest xs)))
;

(defmacro all? (:rest xs)
  (if (empty? xs)
    #true
    `(if (not ,(first xs)) #false (all? ,@(rest xs)))))

(println "(any?) = " (any?))
(println "(any? #false) = " (any? #false))
(println "(any? #true) = " (any? #true))
(println "(any? #false #true) = " (any? #false #true))
(println "(any? #true #false) = " (any? #true #false))
(println "(any? #false #false) = " (any? #false #false))
(println "(any? #true #true) = " (any? #true #true))

(println "(all?) = " (all?))
(println "(all? #false) = " (all? #false))
(println "(all? #true) = " (all? #true))
(println "(all? #false #true) = " (all? #false #true))
(println "(all? #true #false) = " (all? #true #false))
(println "(all? #true #true) = " (all? #true #true))
(println "(all? #false #false) = " (all? #false #false))

