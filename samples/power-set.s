; (power-set ()) => (())
; (power-set '(1)) => ((1) ())
; (power-set '(1 2)) => ((1 2) (1) (2) ())
; (power-set '(1 2 3)) => ((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())

(defun power-set (set)
  (if (empty? set) '(())
    (let ((excludes (power-set (rest set))))
      (concat
        (map (fun (subset) (adjoin (first set) subset)) excludes)
        excludes))))

(print-line "(power-set ()) = " (power-set ()))
(print-line "(power-set '(1)) = " (power-set '(1)))
(print-line "(power-set '(1 2)) = " (power-set '(1 2)))
(print-line "(power-set '(1 2 3)) = " (power-set '(1 2 3)))
