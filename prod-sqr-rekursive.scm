;prod-sqr-rekursive: N x N -> R
; Diese Funktion berechnet das Produkt von i = m bis n
; von i² rekursiv
(define (prod-sqr-rekursive m n)
  (cond
    ;Parameterabfrage: m und n müssen positive ganze Zahlen sein
    ((NOT (AND (integer? m) (integer? n) (> m 0) (> n 0)))
     "Fehler: Nur natürliche Zahlen")
    ((< n m) 0)
    ((= m n) (* m m))
    (else (* (* m m) (prod-sqr-rekursive (+ m 1) n)))))