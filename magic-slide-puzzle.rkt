#lang racket
(require 2htdp/universe 2htdp/image)

(struct tile (pic posn) #:mutable)

(define grid-size 3)
(define tile-size 200)
(define tlc-x 0)
(define tlc-y 50)
(define brc-x (+ tlc-x (* grid-size tile-size)))
(define brc-y (+ tlc-y (* grid-size tile-size)))
(define width (* grid-size tile-size));;;;;;;;;;;;game width // tiles width
(define height brc-y) ;;;;;;;;;;;;;;;final height // total height

(define num-of-moves 0)
(define first-click-position (cons 40 40))
(define second-click-position (cons 53 96))
(define first-clicked? #f)
(define second-clicked? #f)


(define tile-list '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GRAPHICAL OBJECTS AND IMAGES

(define num-of-moves-text (lambda () (text (string-append "NUMBER OF MOVES : " (number->string num-of-moves)) 50 "orange")))

(define background
;  (place-image num-of-moves-text (* .5 width) (/ tlc-y 2)
               (add-line
                (empty-scene width height "black") tlc-x (- tlc-y 1) width (- tlc-y 1) "white"))

(define highlight-square (square tile-size "outline" "yellow"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define initial-state (list (tile (square tile-size "solid" "slateblue") (cons 1 1))
                            (tile (square tile-size "solid" "blue") (cons 1 2))
                            (tile (square tile-size "solid" "yellow") (cons 1 3))
                            (tile (square tile-size "solid" "red") (cons 2 1))
                            (tile (square tile-size "solid" "orange") (cons 2 2))
                            (tile (square tile-size "solid" "black") (cons 2 3))
                            (tile (square tile-size "solid" "green") (cons 3 1))
                            (tile (square tile-size "solid" "slateblue") (cons 3 2))
                            (tile (square tile-size "solid" "forestgreen") (cons 3 3))))
                         

(define (swap pair-of-tiles)
  (begin (set! first-clicked? #f)
         (set! second-clicked? #f)
         (set! num-of-moves (+ num-of-moves 1))
         (cons (tile (tile-pic (cdr pair-of-tiles)) (tile-posn (car pair-of-tiles)))
               (tile (tile-pic (car pair-of-tiles)) (tile-posn (cdr pair-of-tiles))))))
        
;(define (swap list-of-tiles)
;  (let* ((first (tile-from-pos first-click-position))
;         (second (tile-from-pos second-click-position)))
;    (


(define (place tile background)
  (place-image (tile-pic tile)
               (+ tlc-x (- (* (car (tile-posn tile)) tile-size) (/ tile-size 2)))
               (+ tlc-y (- (* (cdr (tile-posn tile)) tile-size) (/ tile-size 2)))
               background))

(define (place-all list-of-tiles)
  (if first-clicked? (place-all-with-highlight list-of-tiles)
      (place-all-without-highlight list-of-tiles)))

(define (place-all-with-highlight list-of-tiles)
  (place-image highlight-square (* (+ (quotient (- (car first-click-position) tlc-x) tile-size) .5) tile-size)
               (+ tlc-y (* (+ (quotient (- (cdr first-click-position)  tlc-y) tile-size) .5) tile-size))
               (place-all-without-highlight list-of-tiles)))
                                  
(define (place-all-without-highlight list-of-tiles)
  (let* ((background-with-score (place-image (num-of-moves-text) (* .5 width) (/ tlc-y 2)
                                             background)))
  (foldr (lambda (x y) (place x y)) background-with-score list-of-tiles)))

(define (dummy list-of-tiles)
  (print "Hello" ))

(define (mouse-function list-of-tiles x y event)
  (case event
    [("button-down")
     (if (not first-clicked?) (begin (set! first-click-position (cons x y))
                                     (set! first-clicked? #t)
                                     list-of-tiles)
         (if (different-region? (cons x y) first-click-position)
             (begin (set! second-click-position (cons x y))
                    (set! second-clicked? #t)
                    list-of-tiles)
             ;(swap list-of-tiles))

             (begin (set! first-clicked? #f)
                    list-of-tiles)))]
    [else list-of-tiles]))

(define (different-region? first second)
  (not (and (= (quotient (- (car first) tlc-x) tile-size) (quotient (- (car second) tlc-x) tile-size))
            (= (quotient (- (cdr first) tlc-y) tile-size) (quotient (- (cdr second) tlc-y) tile-size)))))

(define (tile-from-pos pos list-of-tiles)
  (let* ((grid (cons (+ (quotient (- (car pos) tlc-x) tile-size) 1)
                     (+ (quotient (- (cdr pos) tlc-y) tile-size) 1))))
    (if (null? list-of-tiles) (print "list-of-tiles is null")
        (if (equal? (tile-posn (car list-of-tiles)) grid) (car list-of-tiles)
            (tile-from-pos pos (cdr list-of-tiles))))))

(define (update-list list-of-tiles)
  (if (and first-clicked? second-clicked?)
      (let* ((first-tile (tile-from-pos first-click-position list-of-tiles))
             (second-tile (tile-from-pos second-click-position list-of-tiles))
             ;(mod-x (abs (- (car (tile-posn first-tile)) (car (tile-posn second-tile)))))
             ;(mod-y (abs (- (cdr (tile-posn first-tile)) (cdr (tile-posn second-tile)))))
             (swapped-pair (swap (cons first-tile second-tile))))
       ; (cond (< (+ mod-x mod-y) 2)
              (cons (car swapped-pair) (cons (cdr swapped-pair) (remove first-tile (remove second-tile list-of-tiles)))))
              ;(else list-of-tiles)))
      list-of-tiles))



(define (play-game)
  (big-bang initial-state
            (on-tick update-list 1/25)
            (to-draw place-all)
            (on-mouse mouse-function)))
