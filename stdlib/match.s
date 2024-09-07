
(defun %do-match (exp rule-pairs)
  (if (null? rule-pairs)
    (abort "pattern match failed"))
  (let* ((rule-pair (car rule-pairs))
         (pat (car rule-pair))
         (fn (cadr rule-pair))
         (fail (fun () (%do-match exp (cdr rule-pairs)))))
    (rec loop ((exp exp)
               (pat pat)
               (succeed (fun (args) (apply fn args))))
      (cond
        ((or (number? pat) (string? pat) (boolean? pat))
         (if (= pat exp) (succeed ()) (fail)))
        ((symbol? pat)
         (if (= pat '_) (succeed ()) (succeed (list exp))))
        ((list? pat)
         (cond
           ((null? pat)
            (if (null? exp) (succeed ()) (fail)))
           ((and (symbol? (car pat)) (= (car pat) 'quote))
            (if (= (cadr pat) exp) (succeed ()) (fail)))
           ((and (symbol? (car pat)) (= (car pat) 'record))
            (let ((record-type-name (cadr pat))
                  (field-patterns (cddr pat)))
              (if (not (= (type exp) record-type-name))
                (fail)
                (let ((field-values ((record-destructor (record-descriptor exp)) exp)))
                  (loop (car field-values) (car field-patterns)
                        (fun (car-args)
                          (loop (cdr field-values) (cdr field-patterns)
                            (fun (cdr-args)
                              (succeed (list-concat car-args cdr-args))))))))))
           ((and (list? exp) (not (null? exp)))
            (loop (car exp) (car pat)
                  (fun (car-args)
                    (loop (cdr exp) (cdr pat)
                      (fun (cdr-args)
                        (succeed (list-concat car-args cdr-args)))))))
           (else (fail))))
        (else
          (abort "unrecognized pattern syntax"))))))

; (match exp
;   (pattern body)
;   ...
;   (pattern body))
;
; <pattern> =>   _
;              | <symbol>
;              | <number>
;              | <string>
;              | <boolean>
;              | (quote <datum>)
;              | (<pattern> ...)
;              | 
(defmacro match (exp & rule-pairs)

  (defun get-vars-in (pat)
    (cond
      ((symbol? pat) (if (= pat '_) () (list pat)))
      ((list? pat)
       (cond
         ((null? pat) ())
         ((and (symbol? (car pat)) (= (car pat) 'quote)) ())
         ((and (symbol? (car pat)) (= (car pat) 'record))
          (list-concat (get-vars-in (caddr pat)) (get-vars-in (cdddr pat))))
         (else
          (list-concat (get-vars-in (car pat)) (get-vars-in (cdr pat))))))
      (else
        (abort "unrecognized pattern syntax"))))
  
  `(%do-match ,exp
              (list ,@(list-map (fun (pair)
                                  (let* ((pattern (car pair))
                                         (body (cdr pair))
                                         (ids (get-vars-in pattern)))
                                    `(list ',pattern (fun ,ids ,@body)))) rule-pairs))))
