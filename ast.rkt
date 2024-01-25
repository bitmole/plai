#lang plait

;; Evaluator
;; (calc : (Exp -> Number))
(define-type Exp
  [num (n : Number)]
  [plus (left : Exp) (right : Exp)])

(define (calc e)
  (type-case Exp e
    [(num n) n]
    [(plus l r) (+ (calc l) (calc r))]))

;(print-only-errors #true)
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
;; floating point error
;(test (calc (plus (num 0.1) (num 0.2))) 1/3)

;; 2. Parser
;; (parse : (S-Exp -> Exp))
(define (parse s)
  (cond
    ((s-exp-number? s) (num (s-exp->number s)))
    ((s-exp-list? s)
     (let ([ l (s-exp->list s)])
       (if (symbol=? '+ (s-exp->symbol (first l)))
           (plus (parse (second l)) (parse (third l)))
           (error 'parse "list not an addition"))))))

(test (parse `1) (num 1))
(test (parse `2.3) (num 2.3))
(test (parse `{+ 1 2}) (plus (num 1) (num 2)))
(test (parse `{+ 1 {+ {+ 2 3} 4}})
      (plus (num 1)
            (plus (plus (num 2)
                        (num 3))
                  (num 4))))
(test/exn (parse `{1 + 2}) "symbol")
