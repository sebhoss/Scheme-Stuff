;null: N -> Null
; Funktion gibt den konstanten Wert Null zurück
(define (null) 0)

;succ: N -> N, n -> n + 1
; Funktion gibt den Nachfolger des Eingabewerts zurück
(define (succ n) (+ n 1))

;add: N -> N, m,n -> m + n
; Funktion gibt die Summ aus 2 Werten (m und n) zurück
(define (add m n) (addh m n 0))
 
;Hilfsfunktion für die Funktion "add"
; Es werden die beiden zu addierenden Zahlen,
; sowie ein counter mit Startwert Null
; übergeben
(define (addh zahl1 zahl2 counter)
    (cond 
      ;Wenn der counter den selben Wert hat wie zahl2 wird 
      ;zahl1 als Ergebnis zurückgegeben
      ((= counter zahl2) zahl1)
      ;Falls der counter noch nicht gleich zahl2 ist wird
      ;die Funktion mit dem Nachfolger von zahl1, zahl2 und
      ;dem Nachfolger von counter erneut aufgerufen, solange
      ;bis counter = zahl2 ist.
      (else (addh (succ zahl1) zahl2 (succ counter))))) 

;mult: N -> N, m,n -> m * n
; Funktion gibt das Produkt aus 2 Werten (m und n) zurück
(define (mult m n)
  (cond
    ;Falls einer der Werte Null ist ist das Produkt ebenfalls Null
    ((or (= m 0) (= n 0)) 0)
    ;Falls beide Werte positiv größer Null sind wird die Hilfsfunktion
    ;multh aufgerufen
    (else (multh m n 1 m))))

;Hilfsfunktion für die Funktion "mult"
; Es werden die beiden zu multiplizierenden Zahlen,
; sowie ein counter mit Startwert Null,
; sowie eine Hilfsgröße mit dem Wert von zahl1
; übergeben
(define (multh zahl1 zahl2 counter hilfe)
  (cond
    ;Wenn der counter den selben Wert hat wie zahl2 wird 
    ;zahl1 als Ergebnis zurückgegeben
    ((= counter zahl2) zahl1)
    ;Falls der counter noch nicht gleich zahl2 ist wird
    ;die Funktion mit der Summe aus zahl1 und hilfe, zahl2,
    ;dem Nachfolger von counter, sowie hilfe erneut aufgerufen,
    ;solange bis counter = zahl2 ist.
    (else (multh (add zahl1 hilfe) zahl2 (succ counter) hilfe)))) 

;expon: N -> N, m,n -> m^n
; Funktion gibt das Ergebnis der Rechnung m hoch n wieder
(define (expon m n)
  (cond
    ;Falls m Null ist (also 0^n) ist das Ergebnis immer Null
    ((= m 0) 0)
    ;Falls n Null ist (also m^0) ist das Ergebnis immer Eins
    ;(für m > 0)
    ((= n 0) 1)
    ;Falls beide Werte positiv größer Null sind wird die Hilfsfunktion
    ;exponh aufgerufen
    (else (exponh m n 1 m))))

;Hilfsfunktion für die Funktion "expon"
; Es werden die beiden zu exponenzierenden Zahlen,
; sowie ein counter mit Startwert Null,
; sowie eine Hilfsgröße mit dem Wert von zahl1
; übergeben
(define (exponh zahl1 zahl2 counter hilfe)
  (cond
    ;Wenn der counter den selben Wert hat wie zahl2 wird 
    ;zahl1 als Ergebnis zurückgegeben
    ((= counter zahl2) zahl1)
    ;Falls der counter noch nicht gleich zahl2 ist wird
    ;die Funktion mit dem Produkt von zahl1 und hilfe, zahl2,
    ;dem Nachfolger von counter und hilfe erneut aufgerufen, 
    ;solange bis counter = zahl2 ist.
    (else (exponh (mult zahl1 hilfe) zahl2 (succ counter) hilfe))))