;prod-sqrt: N x N -> R
; Berechnet das Produkt von i = m bis n von der Wurzel aus i/m
(define (prod-sqrt m n)
  ;Das Prodkt wird hier iterativ berechnet
  (define (ifunk m n i ergebnis)
    (cond
      ; Abbruchbedingung: Laufindex i ist gleich n
      ((= i n) (* ergebnis (sqrt (/ i m))))
      ; Falls i noch nicht gleich n ist wird iterativ weitergerechnet
      ((< i n) (ifunk m n (+ i 1) (* ergebnis (sqrt (/ i m)))))))
  (cond
    ;Parameterabfrage: m und n müssen positive ganze Zahlen sein
    ((NOT (AND (> m 0) (> n 0) (integer? m) (integer? n)))
     "Fehler: Werte müssen aus dem Bereich der natürlichen Zahlen sein")
    ; Falls n kleiner m ist wird 0 als Ergebnis zurückgegeben
    ((< n m) 0)
    ; Falls alles in Ordnung ist wird die Funktion iterativ berechnet
    (else (ifunk m n m 1))))
   