
; Standard vector library

(defun vector/map! (f xs)
  (let ((n (length xs))
        (i 0))
    (while (< i n)
      (vector/set! xs i (f (xs i)))
      (set! i (+ 1 i)))))

(defun vector/map (f xs)
  (def xs2 (vector/copy xs))
  (vector/map! f xs2)
  xs2)

(defun vector/for-each (f xs)
  (let* ((n (length xs))
         (i 0))
    (while (< i n)
      (f (xs i))
      (set! i (+ 1 i)))))

(defun vector/filter (f xs)
  (let* ((result [])
         (n (length xs))
         (i 0))
    (while (< i n)
      (if (f (xs i))
        (vector/append! result (xs i)))
      (set! i (+ 1 i)))
    result))

(defun vector/reduce (f i xs)
  (let* ((result i)
         (n (length xs))
         (i 0))
    (while (< i n)
      (set! result (f result (xs i)))
      (set! i (+ 1 i)))
    result))

(def v (apply vector (iota 1 20)))

(vector/filter even? v)

(vector/reduce + 0 v)

