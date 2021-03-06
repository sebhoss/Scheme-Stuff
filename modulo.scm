;mod: N -> N
; mod berechnet n modulo m und gibt das Ergebnis davon aus
(define (mod n m)
  (cond
    ;Falls n und m nicht natürliche Zahlen inklusive Null sind
    ((NOT (AND (integer? n) (integer? m)
               (>= n 0) (>= m 0)))
     "Fehler: Falsche Werte für n oder m")
    ;Falls n kleiner m ist ist n das Ergebnis
    ((< n m) n)
    ;Falls m kleiner-gleich n und n kleiner 2*m dann ist n-m
    ; das Ergebnis
    ((AND (<= m n) (< n (* 2 m))) (- n m))
    ;Falls n größer-gleich 2*m ist ruft sich die Funktion selbst
    ; auf mit mod von n und 2*m als ersten Wert und dem 
    ; unveränderten Wert von m als zweiten Wert
    ((>= n (* 2 m)) (mod (mod n (* 2 m)) m))))


(define (modulo in modul)
  (cond ((>= in modul) (modulo (- in modul) modul))
        ((< in 0) (modulo (+ in modul) modul))
        (else in)))