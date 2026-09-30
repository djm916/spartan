; A small suite of numeric predicate tests, derived from the R7RS spec

(defmacro display (expr)
  (print expr " => ")
  `(println ,expr))

(display (complex? 3.0+4.0i)) ; #t
(display (complex? 3)) ; #t
(display (real? 3)) ; #t
(display (real? -2.5+0.0i)) ; #t
(display (real? -2.5+0.1i)) ; #f
(display (real? 1.0e10)) ; #t
(display (real? +inf)) ; #t
(display (real? NaN)) ; #t
(display (rational? -inf)) ; #f
(display (rational? 3.5)) ; #t
(display (rational? 6/10)) ; #t
(display (rational? 6/3)) ; #t
(display (integer? 3.0+0.0i)) ; #t
(display (integer? 3.0)) ; #t
(display (integer? 8/4)) ; #t

(display (integer? +inf))  ; #false
(display (integer? NaN))   ; #false
(display (complex? +inf))  ; #true
(display (complex? NaN))   ; #true
(display (rational? +inf)) ; #false
(display (rational? NaN))  ; #false
(display (number? NaN))    ; #true
(display (= NaN NaN))      ; #false
(display (= -0.0 0.0))     ; #true
(display (= +inf +inf))    ; #true
(display (= +inf -inf))    ; #false
(display (= -inf -inf))    ; #true

;(display (exact NaN))     ; error

(display (integer? 1.0+0.0i)) ; #true
;(display (exact 1.0+0.0i)) ; error

;(display (exact (complex NaN 0.0)))
(display (integer? (complex +inf 0.0)))
;(display (exact (complex +inf 0.0)))

(display (/ 0.0 0.0)) ; NaN
(display (/ 1.0 0.0)) ; +inf
(display (/ -1.0 0.0)) ; -inf
