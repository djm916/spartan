
(def nums (map string->int sys/args))
(print-line nums)
(print-line "sum = " (fold-left + 0 nums))
(print-line "product = " (fold-left * 1 nums))
