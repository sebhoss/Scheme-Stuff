;max-elem: List -> Z
; Die Funktion max-elem braucht eine Liste als Eingabe und
; liefert das höchste Element der Liste zurück
(define (max-elem Liste)
 (define (hilfemax liste max)
  (cond
    ((NOT (list? Liste)) "Ungültige Eingabe")
    ;Abbruchbedingung: liste leer
    ((null? liste) max)
    ((< max (car liste)) (hilfemax (cdr liste) (car liste)))
    (else (hilfemax (cdr liste) max))))
  (if (null? Liste) '() (hilfemax (hebe-auf Liste) 0)))

;min-max-pair: List -> Pair
; Die Funktion braucht eine Liste als Eingabe und gibt das höchste
; und niedrigste Element als Pair aus
(define (min-max-pair liste)
  ;min-elem: List -> Z
; Die Funktion min-elem braucht eine Liste als Eingabe und
; liefert das niedrigste Element der Liste zurück
(define (min-elem Liste)
 (define (hilfemin liste min)
  (cond
    ((null? liste) min)
    ((> min (car liste)) (hilfemin (cdr liste) (car liste)))
    (else (hilfemin (cdr liste) min))))  
  (if (null? Liste) '() (hilfemin (hebe-auf Liste)
                                  (car (hebe-auf Liste)))))
  (if (NOT (list? liste)) "Ungültige Eingabe"
  (cons (min-elem liste) (max-elem liste))))

;num-of-ints: List -> N
; Die Funktion braucht eine Liste als Eingabe und liefert
; die Anzahl an ganzen Zahlen zurück
(define (num-of-ints liste)
  (cond
    ((NOT (list? liste))
     "Ungültige Eingabe")
    ((null? liste) 0)
    ((list? (car liste)) (+ (num-of-ints (car liste))
                            (num-of-ints (cdr liste))))
    ((integer? (car liste)) (+ 1 (num-of-ints (cdr liste))))
    (else (num-of-ints (cdr liste)))))

;replace: List x Z x Z -> List
; Die Funktion braucht eine Liste und 2 ganze Zahlen als Eingabe
; und ersetzt in dieser Liste z1 mit z2.
(define (replace liste a b)
  (cond
    ((NOT (AND (list? list) (integer? a) (integer? b)))
     "Ungültige Eingabe")
    ((null? liste) '())
    ((list? (car liste)) (cons (replace (car liste) a b)
                               (replace (cdr liste) a b)))
    ((eq? (car liste) a) (cons b (replace (cdr liste) a b)))
    (else (cons (car liste) (replace (cdr liste) a b)))))

;hebe-auf: list -> list
; Die Funktion hebe-auf erwartet eine Liste als Eingabe und
; gibt die Liste ohne eventuelle unter-Listen zurück
(define (hebe-auf liste)
  (cond
    ;Falls die Liste leer ist wird die leere Liste ausgegeben.
    ; Dies wird benutzt um die Funktion später zu terminieren
    ((null? liste) '())
    ;Falls das erste Element der Liste wieder eine Liste ist
    ((list? (car liste))
     ;Hier wird die Funktion haenge-an mitbenutzt um das von 
     ; unter-Listen befreite erste Element der Liste an das von
     ; unter-Listen befreite Rest der Liste zu hängen
     (append (hebe-auf (car liste)) (hebe-auf (cdr liste))))
    ;Falls die Liste nicht leer ist und das erste Element keine
    ; Liste ist wird das erste Element der Liste mit dem von
    ; unter-Listen befreiten Rest der Liste verbunden
    (else (cons (car liste) (hebe-auf (cdr liste))))))