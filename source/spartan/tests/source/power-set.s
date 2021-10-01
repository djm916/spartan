(load "../stdlib/list.txt")

(defun power-set (set)
  (if (= () set)
    '(())
    (concat (power-set (cdr set))
            (list/map (fun (subset) (cons (car set) subset))
                      (power-set (cdr set))))))
