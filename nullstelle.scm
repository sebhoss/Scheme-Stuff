;nullstelle: R -> R
;nullstelle berechnet die Nullstellen der Funktion
; ax² + bx + c = 0
(define (nullstelle a b c)
  (cond 
    ;Falls a gleich Null ist gibt es nur genau eine Nullstelle
    ((= a 0) (simpleX b c))
    ;Falls D kleiner Null ist, ist die Gleichung nicht lösbar
    ((< (D a b c) 0) '(nicht loesbar))
    ;Falls a ungleich Null und D gleich oder größer als Null ist 
    ;gibt es 2 Nullstellen
    (else (list (berechneX1 a b c) (berechneX2 a b c)))) )

;D: R -> R
;D ist b² - 4ac
(define (D a b c) (- (* b b) (* 4 a c)))

;simpleX: R -> R
;simpleX berechnet den Wert x in der gleichung bx + c = 0
(define (simpleX b c) (/ (- c) b))

;berechneX1: R -> R
;x1 ist -b + Wurzel D durch 2a
(define (berechneX1 a b c) (/ (+ (- b) (sqrt(D a b c))) (* 2 a)))

;berechneX2: R -> R
;x2 ist -b - Wurzel D durch 2a
(define (berechneX2 a b c) (/ (- (- b) (sqrt(D a b c))) (* 2 a)))
