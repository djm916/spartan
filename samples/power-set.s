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

(println "(power-set ()) = " (power-set ()))
(println "(power-set '(1)) = " (power-set '(1)))
(println "(power-set '(1 2)) = " (power-set '(1 2)))
(println "(power-set '(1 2 3)) = " (power-set '(1 2 3)))
