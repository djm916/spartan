
(defmacro wrap-io-primitive (fname)
  `(set! ,fname
     (let ((f ,fname))
       (fun (& args)
         (let-values (((result error) (apply f args)))
           (if (not (void? error))
             (raise (make-exception 'io-error error))
             result))))))

(wrap-io-primitive port-open-file)
(wrap-io-primitive port-read)
(wrap-io-primitive port-write)
(wrap-io-primitive port-close)
(wrap-io-primitive port-open?)
(wrap-io-primitive port-position)
(wrap-io-primitive port-seek)
(wrap-io-primitive port-size)
