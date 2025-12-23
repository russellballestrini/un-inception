(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(do ((i 0 (+ i 1)))
    ((> i 10))
  (display (string-append "fib(" (number->string i) ") = " (number->string (fib i)) "\n")))
