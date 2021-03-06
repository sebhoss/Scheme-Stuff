;haenge-an: list x list -> list
; Die Funktion haenge-an erwartet 2 Listen als Eingabe und hängt
; die 2. Liste hinter die erste Liste
(define (haenge-an liste1 liste2)
  (cond
    ;Falls liste1 & liste2 leere Listen sind wird nichts gemacht
    ; bzw. die leere Liste angehangen und die Funktion damit beendet
    ((AND (null? liste1)(null? liste2)) '())
    ;Falls liste1 noch nicht leer ist
    ((NOT (null? liste1))
     ;Es wird das erste Element von liste1 mit dem Rest der Liste
     ; und liste2 verbunden. Dies geschieht rekursiv
     (cons (car liste1) (haenge-an (cdr liste1) liste2)))
    ;Falls liste2 noch nicht leer ist
    ((NOT (null? liste2))
     ;Es wird das erste Element von liste2 mit dem Rest der Liste
     ; und liste1 verbunden. Dies geschieht rekursiv
     (cons (car liste2) (haenge-an liste1 (cdr liste2))))))

;drehe-um: list -> list
; Die Funktion drehe-um erwartet eine Liste als Eingabe und 
; gibt diese in umgekehrter Reihenfolge wieder aus
(define (drehe-um liste)
  ;Falls die Liste leer ist wird die leere Liste ausgegeben.
  ; Dies wird benutzt um die Funktion später zu terminieren
  (if (null? liste) '()
      ;Zur Unterstützung wird hier die Funktion haenge-an
      ; mitbenutzt. Es wird der (zukünftig umgedrehte) Rest
      ; der Liste an das 1. Element der Liste sowie an die leere
      ; Liste gehangen.
      ; (Die leere Liste ist daher notwendig, da cons ein pair 
      ; erwartet und bei der Übergabe der Parameter an haenge-an
      ; dies so ohne weiteres nichtmehr gewährleistet ist)
      (haenge-an (drehe-um (cdr liste)) (cons (car liste) '()))))

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
     (haenge-an (hebe-auf (car liste)) (hebe-auf (cdr liste))))
    ;Falls die Liste nicht leer ist und das erste Element keine
    ; Liste ist wird das erste Element der Liste mit dem von
    ; unter-Listen befreiten Rest der Liste verbunden
    (else (cons (car liste) (hebe-auf (cdr liste))))))

;aufhebendes-haenge-an: list x list -> list
; Diese Funktion erwartet 2 Listen als Eingabe. Diese werden
; zuerst zu einer Liste verbunden und anschließend ohne eventuelle
; unter-Listen ausgegeben
(define (aufhebendes-haenge-an liste1 liste2)
  ;Da diese Funktion zwar 2 Listen als Eingabe erfordert, aber nur
  ; eine ausgibt wird zuerst haenge-an benutzt um aus den 2 Listen
  ; eine zu machen und anschließend hebt hebe-auf mögliche unter-
  ; Listen auf
  (hebe-auf (haenge-an liste1 liste2)))

;aufhebendes-drehe-um: list -> list
; Diese Funktion erwartet eine Liste als Eingabe. Diese wird
; zuerst in eine Liste ohne eventuelle unter-Listen umgewandelt
; und anschließend in umgekehrter Reihenfolge zurückgegeben
(define (aufhebendes-drehe-um liste)
  ;Zuerst wird die Liste von möglichen unter-Listen befreit, 
  ; anschließend in umgekehrter Reihenfolge ausgegeben
  (drehe-um (hebe-auf liste)))