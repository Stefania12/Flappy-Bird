#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "random.rkt")
(require "abilities.rkt")
(require "constants.rkt")
 (require racket/trace)
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)


(struct State (dank-birb pipes var abilities score))
(struct Birb (x y v-y) #:transparent)
(struct Pipe (x gap-y) #:transparent)
(struct Variables (gravity momentum scroll-speed) #:transparent)
(struct Abilities (visible active) #:transparent)


(define (get-random-pipe-gap-y)
  (+ added-number (random random-threshold)))

(define (get-initial-state)
  (let ((initial-bird (Birb bird-x bird-initial-y 0)) (var (Variables initial-gravity initial-momentum initial-scroll-speed)))
    (State initial-bird (list (Pipe scene-width (get-random-pipe-gap-y))) var (Abilities null null) 0)))


(define (get-bird state) (State-dank-birb state))
(define (get-bird-y bird) (Birb-y bird))
(define (get-bird-x bird) (Birb-x bird))


(define (next-state-bird bird gravity)
  (Birb (get-bird-x bird) (+ (get-bird-y bird) (get-bird-v-y bird)) (+ gravity (get-bird-v-y bird))))


(define (get-bird-v-y bird) (Birb-v-y bird))


(define (next-state-bird-onspace bird momentum)
  (Birb (get-bird-x bird) (get-bird-y bird) (* -1 momentum)))


(define (change current-state pressed-key)
  (if (key=? pressed-key " ")
      (let* ((var (get-variables current-state)) (g (get-variables-gravity var)) (mom (get-variables-momentum var)) (scrl-speed (get-variables-scroll-speed var)))
        (State (next-state-bird-onspace (get-bird current-state) mom) (get-pipes current-state) var (get-abilities current-state) (get-score current-state)))
      current-state))


(define (get-pipes state) (State-pipes state))


(define (get-pipe-x pipe) (Pipe-x pipe))
(define (get-pipe-gap-y pipe) (Pipe-gap-y pipe))


(define (move-pipes pipes scroll-speed)
  (map (λ(pipe)(Pipe (- (get-pipe-x pipe) scroll-speed) (get-pipe-gap-y pipe))) pipes))


(define (clean-pipes pipes)
  (filter (λ(pipe)(> (+ pipe-width (get-pipe-x pipe)) 0)) pipes))


(define (add-more-pipes pipes)
  (if (< (length pipes) no-pipes)
      (let* ((last-pipe (last pipes)) (new-pipe-x (+ (get-pipe-x (car pipes)) pipe-width pipe-gap)))
        (cons (Pipe new-pipe-x (get-random-pipe-gap-y)) pipes))
      pipes))


(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))


(define (get-score state) (State-score state))


(define (check-ground-collision bird)
 (>= (+ bird-height (get-bird-y bird)) ground-y))



(define (invalid-state? state)
  (let ((dank-birb (get-bird state)))
   (or (check-ground-collision dank-birb) (check-pipe-collisions (get-bird state) (get-pipes state)))))


(define (check-pipe-collisions bird pipes)
  (let* ((bird-x (get-bird-x bird)) (bird-y (get-bird-y bird)) (left-corner-bird (make-posn bird-x bird-y)) (right-corner-bird (make-posn (+ bird-x bird-width) (+ bird-y bird-height))))
    (not (null? (filter (λ(pipe)
              (let* ((x (get-pipe-x pipe))(y (get-pipe-gap-y pipe))(left-corner-up (make-posn x (- y pipe-height)))(right-corner-up (make-posn (+ x pipe-width) y))
                     (left-corner-low (make-posn x (+ y pipe-self-gap)))(right-corner-low (make-posn (+ x pipe-width) pipe-height)))
                (or (check-collision-rectangles left-corner-bird right-corner-bird left-corner-up right-corner-up)
                    (check-collision-rectangles left-corner-bird right-corner-bird left-corner-low right-corner-low)))) pipes)))))

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))


(define (next-state state)
  (let* ((var (get-variables state)) (g (get-variables-gravity var)) (scroll-speed (get-variables-scroll-speed var))(abilities (get-abilities state))(bird (get-bird state)))
    (let ((initial-variables  (Variables initial-gravity initial-momentum initial-scroll-speed))) 
      (State (next-state-bird bird g) (next-state-pipes (get-pipes state) scroll-speed) (next-variables initial-variables (get-abilities-active abilities)) (next-abilities abilities bird scroll-speed) (+ 0.1 (get-score state))))))


(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))
(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))

(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))


(define (draw-frame state)
  (let* ((birb (get-bird state)) (birb-offset-x (quotient bird-width 2)) (birb-offset-y (quotient bird-height 2))
                                (birb-center-x (+ (get-bird-x birb) birb-offset-x)) (birb-center-y (+ (get-bird-y birb) birb-offset-y)))
    (let ((score (get-score state))(pipes (get-pipes state)))
      (let* ((ground-center-x (quotient scene-width 2)) (ground-offset-y (quotient ground-height 2)) (ground-center-y (+ ground-y ground-offset-y)))
        (let ((score-and-pipes (place-image (score-to-image score) text-x text-y (place-pipes pipes initial-scene))))
          (let ((visible (get-abilities-visible (get-abilities state))) (active (get-abilities-active (get-abilities state))))
            (let ((place-abilities (λ(scene) (place-active-abilities active (place-visible-abilities visible scene)))))
              (place-abilities (place-image bird-image birb-center-x birb-center-y (place-image ground-image ground-center-x ground-center-y score-and-pipes))))))))))
  

(define (place-pipes pipes scene)
  (let ((pipe-image (rectangle pipe-width pipe-height "solid" "green")) (offset-x (quotient pipe-width 2)) (offset-y (quotient pipe-height 2)))
    (let ((upper-pipes-coordinates (map (λ(p)(make-posn (+ offset-x (get-pipe-x p)) (- (get-pipe-gap-y p) offset-y))) pipes))
          (lower-pipes-coordinates (map (λ(p)(make-posn (+ offset-x (get-pipe-x p)) (+ (get-pipe-gap-y p) pipe-self-gap offset-y))) pipes)))
      (place-images (make-list (* 2 (length pipes)) pipe-image) (append upper-pipes-coordinates lower-pipes-coordinates) scene))))


(define slow-ability
  (let ((new-scroll-speed-func (λ(var)(Variables (get-variables-gravity var) (get-variables-momentum var)(add1 (get-variables-scroll-speed var))))))
    (Ability (hourglass "tomato") 30 null new-scroll-speed-func)))

(define fast-ability
  (let ((new-scroll-speed-func (λ(var)(Variables (get-variables-gravity var) (get-variables-momentum var) (max 5 (sub1 (get-variables-scroll-speed var)))))))
    (Ability (hourglass "mediumseagreen") 10 null new-scroll-speed-func)))


(define ABILITIES (list slow-ability fast-ability))

(define (get-variables state) (State-var state))
(define (get-variables-gravity var) (Variables-gravity var))
(define (get-variables-momentum var) (Variables-momentum var))
(define (get-variables-scroll-speed var) (Variables-scroll-speed var))


(define (get-abilities x) (State-abilities x))
(define (get-abilities-visible x) (Abilities-visible x))
(define (get-abilities-active x) (Abilities-active x))


(define (clean-abilities visible)
  (let ((is-ability-visible? (λ(a)(>= (+ (get-ability-pos-x a) 20) 0))))
    (filter is-ability-visible? visible)))


(define (move-abilities visible scroll-speed)
  (let ((move-ability (λ(a)(Ability (get-ability-image a) (get-ability-time a) (make-posn (- (get-ability-pos-x a) scroll-speed) (get-ability-pos-y a)) (get-ability-next a)))))
    (map move-ability visible)))


(define (time-counter active)
  (let ((is-ongoing? (λ(a)(> (get-ability-time a) 0))) (time-passed (/ 1.0 fps)))
    (let ((pass-time (λ(a)(Ability (get-ability-image a) (- (get-ability-time a) time-passed) (get-ability-pos a) (get-ability-next a)))))
      (filter is-ongoing? (map pass-time active)))))


(define (next-abilities-visible visible scroll-speed)
  (let* ((abilities-left (clean-abilities (move-abilities visible scroll-speed))))
    (fill-abilities abilities-left  DISPLAYED_ABILITIES ABILITIES)))

(define (next-abilities abilities bird scroll-speed)
  (let ((visible (get-abilities-visible abilities))(active (get-abilities-active abilities)))
    (let ((bird-x (get-bird-x bird)) (bird-y (get-bird-y bird)))
      (let ((bird-left-corner (make-posn bird-x bird-y)) (bird-right-corner (make-posn (+ bird-x bird-width) (+ bird-y bird-height))))
        (let ((get-ability-left-corner (λ(a)(make-posn (- (get-ability-pos-x a) 20) (- (get-ability-pos-y a) 20)))) (get-ability-right-corner (λ(a)(make-posn (+ (get-ability-pos-x a) 20) (+ (get-ability-pos-y a) 20)))))
          (let ((collides-with-bird? (λ(a)(check-collision-rectangles bird-left-corner bird-right-corner (get-ability-left-corner a) (get-ability-right-corner a)))))
            (let-values ([(colliding not-colliding)(partition collides-with-bird? visible)])
              (Abilities (next-abilities-visible not-colliding scroll-speed) (append (time-counter active) colliding)))))))))

(define (next-variables variables abilities)
  (if (null? abilities)
      variables
      ((compose-abilities abilities) variables)))


(define (place-visible-abilities abilities scene)
  (let ((images (map (λ(a)(get-ability-image a)) abilities)) (positions (map (λ(a)(get-ability-pos a)) abilities)))
    (place-images images positions scene)))


(define (place-active-abilities abilities scene)
  (let* ((indexes (range (length abilities))) (positions (map (λ(x)(make-posn (- (posn-x abilities-posn) (* 50 x)) (posn-y abilities-posn))) indexes)))
    (let ((images (map (λ(a)(scale 0.75 (get-ability-image a))) abilities)))
      (place-images images positions scene))))


(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
