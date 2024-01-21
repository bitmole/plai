#lang plait

(define-type Exp
  [num (n : Number)]
  [plus (left : Exp) (right : Exp)])

; evaluator type:
;(calc : (Exp -> Number))
(define (calc e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (calc l) (calc r))]))

(print-only-errors #true)
(test (calc (num 1)) 1)
(test (calc (num 2.3)) 2.3)
(test (calc (plus (num 1) (num 2)))
      3)
(test (calc (plus (plus (num 1) (num 2)) (num 3)))
      6)
(test (calc (plus (plus
                   (plus (num 1) (num 2))
                   (num 3))
                  (num 4)))
      10)
(test (calc (plus (num 0.1) (num 0.2))) 0.3)
(test (calc (plus (num 0.1) (num 0.2))) 1/3)