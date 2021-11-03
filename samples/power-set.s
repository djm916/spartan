(load "stdlib/list.txt")

; (power-set ()) => (())
; (power-set '(1)) => ((1) ())
; (power-set '(1 2)) => ((1 2) (1) (2) ())
; (power-set '(1 2 3)) => ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())

(defun power-set (set)
  (if (= () set) '(())
    (let ((excludes (power-set (cdr set))))
      (concat (list/map
                (fun (subset) (cons (car set) subset))
                excludes)
              excludes))))
