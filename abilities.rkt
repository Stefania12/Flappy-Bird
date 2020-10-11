#lang racket

(provide fill-abilities)
(provide compose-abilities)
(provide hourglass)
(provide get-ability-image)
(provide get-ability-time)
(provide get-ability-pos)
(provide get-ability-pos-x)
(provide get-ability-pos-y)
(provide get-ability-next)
(provide struct Ability)

(require "random.rkt")
(require lang/posn)
(require 2htdp/image)
(require racket/trace)

; Imaginea si range-ul în care vor aparea abilitațile
; Nu modificați
(define POSITION_RANGE '((300 2000) (30 550)))
(define (hourglass color) (underlay
 (rectangle 40 40 "solid" color)
 (polygon
  (list (make-posn 0 0)
        (make-posn 25 0)
        (make-posn 0 25)
        (make-posn 25 25))
  "outline"
  (make-pen "darkslategray" 5 "solid" "round" "round"))))

(struct Ability (image time pos next) #:transparent)

(define (get-ability-image ability) (Ability-image ability))
(define (get-ability-time  ability) (Ability-time ability))
(define (get-ability-pos   ability) (Ability-pos ability))
(define (get-ability-pos-x ability) (posn-x (get-ability-pos ability)))
(define (get-ability-pos-y ability) (posn-y (get-ability-pos ability)))
(define (get-ability-next  ability) (Ability-next ability))

; Returneaza o poziție aleatorie în POSITION_RANGE.
(define (random-position range)
	(apply make-posn (map ((curry apply) random) range)))

; Returnează o listă de n elemente alese aleatoriu din lista L.
(define (choice-abilities n L)
	(sample (discrete-dist L) n))

; Va parcurge abitatile și pentru cele care au poziția null va asigna
; una aletorie.
; Folosiți random-position
(define (position-abilities abilities)
  (let ((assign-pos (λ(a)(if (null? (get-ability-pos a)) (Ability (get-ability-image a) (get-ability-time a) (random-position POSITION_RANGE) (get-ability-next a)) a))))
    (map assign-pos abilities)))
    
; Fiecare abilitate are o funcție next care modifica stare jocului
; Compuneti toate funcțiile next în una singură
; Hint: compose
(define (compose-abilities L)
  (let ((functions (map (λ(a)(get-ability-next a)) L)))
    (foldl (λ(x acc)(compose x acc)) (λ(x)x) functions)))

; Primiște o listă de abilități inițiale, un număr n
; și o listă cu toate abilități posibile.
; Va adauga elemente aleatoare la lista inițială pană aceasta are lungimea n
; Atentie n poate fi chiar si 0 cand vrem sa jucam fara nicio abilitate.
; Folosiți choice-abilities.
(define (fill-abilities initial n abilities)
  (let ((abilities-to-add (- n (length initial))))
    (position-abilities (append (choice-abilities abilities-to-add abilities) initial))))