;russmult: N -> N
;russmult multipliziert 2 natürliche Zahlen und gibt
;das Ergebnis zurück. Die erste Zahl wird dabei immer
;ganzzahlig halbiert während die zweite Zahl verdoppelt
;wird. Das Ergebnis ergibt sich dann aus der Summe der Zeilen
;in einer Tabelle mit den Werten für m und n, in denen m ungerade
;ist. Die Funktion ruft dazu eine Hilfsfunktion mit den beiden
;Werten sowie der 0 als Parameter auf.
(define (russmult m n) (russmulth m n 0))

;russmulth ist eine Hilfsfunktion zu russmult
;Es werden dabei die zu multiplizierenden Zahlen
;sowie eine Hilfsvariable mit dem Startwert Null
;übergeben
(define (russmulth zahl1 zahl2 hilfe)
  (cond
    ;Wenn eine der Zahlen Null ist ist Null das Ergebnis
    ((OR (= zahl1 0) (= zahl2 0)) 0)
    ;Wenn die erste Zahl = 1 ist ist das Ergebnis
    ;zweite Zahl + Hilfszahl
    ((= zahl1 1) (+ hilfe zahl2))
    ;Wenn die erste Zahl noch ungleich 1 ist und nicht
    ;glatt durch 2 teilbar wird die Funktion erneut aufgerufen
    ;zahl1 wird gerade gemacht,
    ;zahl2 nicht verändert,
    ;Die Hilfszahl wird um zahl2 erhöht, da in diesem Schritt
    ;zahl1 ungerade war
    ((= (remainder zahl1 2) 1) (russmulth (- zahl1 1) zahl2 (+ zahl2 hilfe)))
    ;War zahl1 ohne Rest durch 2 teilbar wird zahl1 halbiert, zahl2
    ;verdoppelt und die hilfszahl nicht verändert.
    (else (russmulth (/ zahl1 2) (* zahl2 2) hilfe)) ))
