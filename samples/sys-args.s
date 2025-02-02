
; Example of using sys/args to get command-line arguments
; Takes a list of integers as command-line arguments and prints their sum and product

(def nums (map string->int *command-line-args*))
(print-line nums)
(print-line "sum = " (fold-left + 0 nums))
(print-line "product = " (fold-left * 1 nums))
