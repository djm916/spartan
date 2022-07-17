
; (derivative f) returns a function that approximates the derivative f' of some function f.
; Recall that:
;
;               f(x + dx) - f(x)
; f'(x) = lim   ----------------
;         dx->0        dx
;
; Whereas f' is an exact equation for the slope of the tangent line at any point x of f,
; this function gives an approximation of f' with a secant line. That is:
;
;          f(x + dx) - f(x)
; f'(x) ~= ----------------
;                 dx
;
; for *sufficiently small* values of dx.

(defun derivative (f)
  (def dx 0.1)
  (fun (x) (/ (- (f (+ x dx)) (f x)) dx)))

; Newton's method to find the zeros of an arbitrary function f. That is, to find an x such that f(x) = 0.
;
; Given an initial estimate x0, we can generate increasingly better estimates by iterating the recurrence relation:
;
;               f(x_n)
; x_n+1 = x_n - -------
;               f'(x_n)
;
; where x_n+1 is a better estimate than x_n.

(defun newton (f x0)
  (def df (derivative f)) ; df ~= f', an approximation of the derivative function of f
  (def done false)        ; Set to true when the method converges, ending the iteration
                          ; (i.e., the difference between successive approximations
                          ;  is less than some small tolerance value)
  (def x1 nil)            ; If the method converges, x1 is a better estimate than x0,
                          ; and also stores the final result
  ; Iterate Newton's method
  (while (not done)
    (set! x1 (- x0 (/ (f x0) (df x0))))
    ;(print-line x1)
    (if (<= (abs (- x1 x0)) 0.00001) ; Compute the difference between successive approximations
      (set! done true)               ; If the method converges, then x1 is the result and we are done
      (set! x0 x1)))                 ; Otherwise, continue the iteration using the new estimate
  x1)

; Example: Use Newton's method to compute sqrt(x), i.e., find the zeros of f(y) = x - y^2.
; Let y = sqrt(x)
; y^2 = x
; 0 = x - y^2

(defun sqrt (x) (newton (fun (y) (- x (* y y))) 1.0))

(print-line "The square root of 2 is " (sqrt 2.0))
