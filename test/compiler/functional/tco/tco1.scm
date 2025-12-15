(define (countdown n)
  (if (eq? n 0)
      0
      (countdown (- n 1))))

(countdown 10000000)