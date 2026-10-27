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
(display (real? +inf.0)) ; #t
(display (real? +nan.0)) ; #t
(display (rational? -inf.0)) ; #f
(display (rational? 3.5)) ; #t
(display (rational? 6/10)) ; #t
(display (rational? 6/3)) ; #t
(display (integer? 3.0+0.0i)) ; #t
(display (integer? 3.0)) ; #t
(display (integer? 8/4)) ; #t

(display (integer? +inf.0))  ; #false
(display (integer? +nan.0))   ; #false
(display (complex? +inf.0))  ; #true
(display (complex? +nan.0))   ; #true
(display (rational? +inf.0)) ; #false
(display (rational? +nan.0))  ; #false
(display (number? +nan.0))    ; #true
(display (= +nan.0 +nan.0))      ; #false
(display (= -0.0 0.0))     ; #true
(display (= +inf.0 +inf.0))    ; #true
(display (= +inf.0 -inf.0))    ; #false
(display (= -inf.0 -inf.0))    ; #true

;(display (exact +nan.0))     ; error

(display (integer? 1.0+0.0i)) ; #true
;(display (exact 1.0+0.0i)) ; error

;(display (exact (complex +nan.0 0.0)))
(display (integer? (complex +inf.0 0.0)))
;(display (exact (complex +inf.0 0.0)))

(display (/ 0.0 0.0)) ; +nan.0
(display (/ 1.0 0.0)) ; +inf.0
(display (/ -1.0 0.0)) ; -inf.0
