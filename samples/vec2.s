
(in-package math2d)

(defstruct vec2 (x y))

(defun + (v1 v2)
  (make-vec2 (spartan.core:+ (v1 'x) (v2 'x)) (spartan.core:+ (v1 'y) (v2 'y))))

(in-package user)

(import math2d)

(def v1 (make-vec2 0.0 0.0))
(def v2 (make-vec2 1.0 1.0))
(def v3 (+ v1 v2))
(print-line "v3 = (" (v3 'x) ", " (v3 'y) ")")
