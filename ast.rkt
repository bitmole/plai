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