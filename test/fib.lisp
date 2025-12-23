(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(loop for i from 0 to 10
      do (format t "fib(~d) = ~d~%" i (fib i)))
