;sum-rekursive: N x N -> R
; Berechnet die Summe von i = m bis n von (i + m)/(i + 1)² rekursiv
(define (sum-rekursive m n)
  (define (hilf m n i)
    (cond
      ((= i n) (/ (+ i m) (* (+ i 1) (+ i 1))))
      ((< i n) (+ (/ (+ i m) (* (+ i 1) (+ i 1))) (hilf m n (+ i 1))))))
  (cond
    ((NOT (AND (integer? m) (integer? n) (> m 0) (> n 0)))
     "Fehler: Nur natürliche Zahlen")
    ((< n m) 0)
    (else (hilf m n m))))