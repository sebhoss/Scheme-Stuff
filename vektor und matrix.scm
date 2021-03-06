;add: Vektor x Vektor -> Vektor 
; Die Funktion add addiert zwei Vektoren und gibt einen
; neuen Vektor wieder aus.
(define (add Vektor1 Vektor2)
  (define (vektor? vektor)
    (cond
      ((null? vektor) #t)
      ((AND (list? vektor) (real? (car vektor)))
       (vektor? (cdr vektor)))
      (else #f)))
  (cond 
    ;Hier werden die Übergebenen Parameter geprüft
    ((NOT (AND (list? Vektor1) (list? Vektor2)
               (vektor? Vektor1) (vektor? Vektor2)))
     "Ungültige Eingabe")
    ((null? Vektor2) Vektor1)
    ((null? Vektor1) Vektor2)
    ;Hier werden die beiden Vectoren addiert
    (else (cons (+ (car Vektor1) (car Vektor2))
                (add (cdr Vektor1) (cdr Vektor2))))))

;addm: Matrix x Matrix -> Matrix
; Die Funktion addiert zwei Matrizen zeilenweise und gibt eine
; Matrix als Ergebnis zurück.
(define (addm Matrix1 Matrix2)
  (define (matrix? matrix)
    (define (vektor? vektor)
     (cond
       ((null? vektor) #t)
       ((AND (list? vektor) (real? (car vektor)))
        (vektor? (cdr vektor)))
       (else #f)))
    (cond
      ((null? matrix) #t)
      ((AND (list? matrix)
            (list? (car matrix))
            (vektor? (car matrix)))
                   (matrix? (cdr matrix)))
      (else #f)))  
  (cond
    ;Parameter prüfen
    ((NOT (AND (matrix? Matrix1) (matrix? Matrix2)))
     "Ungültige Eingabe")
    ((null? Matrix1) Matrix2)
    ((null? Matrix2) Matrix1)
    ;Addition einzelner Zeilen mit Hilfe der Funktion add
    (else (cons (map + (car Matrix1) (car Matrix2))
                (addm (cdr Matrix1) (cdr Matrix2))))))


;skalmult: Skalar x Vektor -> Vektor
; Es wird ein Skalar mit einem Vektor multipliziert
(define (skalmult Skalar Vektor)
  (define (vektor? vektor)
    (cond
      ((null? vektor) #t)
      ((AND (list? vektor) (real? (car vektor)))
       (vektor? (cdr vektor)))
      (else #f)))
  (cond
    ;Parameterabfrage
    ((NOT (AND (real? Skalar) (list? Vektor) (vektor? Vektor)))
     "Ungültige Eingabe")
    ;Abbruchbedingung: Vektor ist leer
    ((null? Vektor) '())
    ;Hier wird zeilenweise der Vektor mit dem Skalar multipliziert
    (else (cons (* Skalar (car Vektor))
                (skalmult Skalar (cdr Vektor))))))

;skalmultm: Skalar x Matrix -> Matrix
; Die Funktion skalarmultipliziert eine Matrix mit gegebenen Skalar
(define (skalmultm skalar matrix)
  (define (matrix? matrix)
    (define (vektor? vektor)
     (cond
       ((null? vektor) #t)
       ((AND (list? vektor) (real? (car vektor)))
        (vektor? (cdr vektor)))
       (else #f)))
    (cond
      ((null? matrix) #t)
      ((AND (list? matrix)
            (list? (car matrix))
            (vektor? (car matrix)))
                   (matrix? (cdr matrix)))
      (else #f)))  
  (cond
    ((NOT (AND (real? skalar) (matrix? matrix)))
     "Ungültige Eingabe")
    ;Abbruchbedingung: Matrix ist leer
    ((null? matrix) '())
    (else (cons (skalmult skalar (car matrix))
                (skalmultm skalar (cdr matrix))))))

;mult: Matrix x Matrix -> Matrix
; Die Funktion mult fordert 2 Matrizen als Eingabe, überprüft ob diese
; multiplizierbar sind und multipliziert diese dann oder gibt eine
; Fehlermeldung aus.
(define (mult matrix1 matrix2)
  ;transponier: Matrix -> Matrix
  ; Die Funktion transponier transponiert eine Matrix
  (define (transponier matrix)
  (if (eq? (length (car matrix)) 1) (list (map car matrix))
      (append (list (map car matrix))
              (transponier (map cdr matrix)))))
  ;multhilfe: Matrix -> Matrix
  ; Die Funktion multhilfe führt die eigentliche Berechnung der Matrix-
  ; multiplikation durch. Es wird Zeile mal Spalte gerechnet, die Ergebnise
  ; addiert und anschließend als neue Matrix ausgegeben
  (define (multhilfe matrix1 matrix2)
    ;multandadd: Matrix x Matrix -> Matrix
    ; Die Funktion multandadd multipliziert eine Zeile einer Matrix mit 
    ; der Spalte der anderen Matrix und addiert das Ergebnis
   (define (multandadd zeile spalte)
    (apply + (map * zeile spalte)))
    (cond
      ((null? matrix1) '())
      (else (append 
          (list (map (lambda (z) (multandadd (car matrix1) z))
                            (transponier matrix2)))
          (multhilfe (cdr matrix1) matrix2)))))
  (define (matrix? matrix)
    (define (vektor? vektor)
     (cond
       ((null? vektor) #t)
       ((AND (list? vektor) (real? (car vektor)))
        (vektor? (cdr vektor)))
       (else #f)))
    (cond
      ((null? matrix) #t)
      ((AND (list? matrix)
            (list? (car matrix))
            (vektor? (car matrix))))
       (matrix? (cdr matrix))
      (else #f)))
  (cond
    ((NOT (AND (matrix? matrix1) (matrix? matrix2)))
     "Matrizen können nicht multipliziert werden, da eine der beiden eingegebenen Matrizen nicht wirklich eine Matrix ist")
    ((NOT (AND (eq? (length matrix1) (length matrix2))))
     "Matrizen können nicht multipliziert werden, da die Anzahl der Zeilen von Matrix 1 nicht mit der Anzahl an Spalten von Matrix 2 übereinstimmen")
    (else (multhilfe matrix1 matrix2))))