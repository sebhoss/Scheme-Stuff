;binom: N -> N
;binom berechnet den Binominal-Koeffizienten von n über k
(define (binom n k)
  (cond
    ;Falls n = k oder k = 0 ist ist das Ergebnis 1
    ((OR (= n k) (= k 0)) 1)
    ;Falls n kleiner k ist ist das Ergebnis 0
    ((< n k) 0)
    ;Falls n größer als k und k größer gleich 1 ist ist das Ergebnis
    ;die Summe aus 2 neuen Binominal-Koeffizienten, deren n und k
    ;Werte verändert wurden
    ((AND (> n k) (>= k 1)) (+ (binom (- n 1) (- k 1)) (binom (- n 1) k)))))