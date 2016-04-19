#lang plai-typed

;;Seçil Yıldırgı 111200020
;;CMPE 314 - ASSINGMENT 5

;;msl -> number
;;msl -> msl+msl
;;msl -> msl*msl
;;msl ->(msl)
;; op: [+, *, **, -]

(define-type msl
  [msl-num (n : number)]
  [msl-add (lhs : msl) (rhs : msl)]
  [msl-mul (lhs : msl) (rhs : msl)]
  [msl-sub (lhs : msl)]
  [msl-expo (lhs : msl) (rhs : msl)]
  [msl-checkZ (exp1 : msl) (exp2 : msl) (exp3 : msl)]
  [msl-fact (lhs : msl)])
  

;;exponentiation function
(define(** num1 num2)
  (cond
    ((= num1 1) num2)
    (else
     (* num2(**(sub1 num1) num2)))))

;;if greater than zero function
(define (CheckZ exp1 exp2 exp3)
  (cond
    ((> exp1 0) exp2)
    ((< exp1 0) exp3)))
    

;;cal msl --> number
;;calculate some values using a msl expression
;;examples
;;(msl-num 9) -> 9
;;(msl-add (msl-num 7) (msl-num 3) --> 10
;;(msl-add (msl-add (msl-num -6) (msl-num 4)) (msl-mul (msl-num 2) (msl-num 7)) --> 12
;;(msl-mul (msl-sub (msl-num 4)) (msl-add (msl-num 3) (msl-num 5))) --> -32
;;(msl-checkZ (msl-num 5) (msl-num 10) (msl-num 20)) --> 10
(define (cal [expr : msl])
  (type-case msl expr
    [msl-num (n) n]
    [msl-add (lhs rhs) (+ (cal lhs) (cal rhs))]
    [msl-mul (lhs rhs) (* (cal lhs) (cal rhs))]
    [msl-sub (lhs) (* (cal lhs) -1)]
    [msl-expo (lhs rhs) (** (cal lhs) (cal rhs))]
    [msl-checkZ (exp1 exp2 exp3) (CheckZ (cal exp1) (cal exp2) (cal exp3))]
    [msl-fact (lhs) (factorial (cal lhs))]
    ))

;;test
(test (cal (msl-num 9))  9)
(test (cal (msl-add (msl-num 7) (msl-num 3)))  10)
(test (cal (msl-add (msl-add (msl-num -6) (msl-num 4)) (msl-mul (msl-num 2) (msl-num 7)))) 12)
(test (cal (msl-mul (msl-sub (msl-num 4)) (msl-add (msl-num 3) (msl-num 5)))) -32)
(test (cal (msl-mul (msl-sub (msl-num 15)) (msl-add (msl-num 30) (msl-num 10)))) -600)
(test (cal (msl-sub (msl-mul (msl-num 8) (msl-num 8)))) -64)
(test (cal (msl-add (msl-sub (msl-num 15)) (msl-add (msl-num 4) (msl-num 8)))) -3)
(test (cal (msl-checkZ (msl-num 5) (msl-num 10) (msl-num 20))) 10)
(test (cal (msl-checkZ (msl-num -5) (msl-num 10) (msl-num 20))) 20)
(test (cal (msl-checkZ (msl-num 4) (msl-num 30) (msl-num 20))) 30)
(test (cal (msl-checkZ (msl-num -8) (msl-num 100) (msl-num 356))) 356)
(test (cal (msl-expo (msl-num 3) (msl-num 2))) 8)
(test (cal (msl-expo (msl-num 3) (msl-num 3))) 27)
(test (cal (msl-expo (msl-num 5) (msl-num 2))) 25)
(test (cal (msl-expo (msl-num 1) (msl-num 15))) 1)
(test (cal (msl-fact (msl-num 4))) 24)

;;--------------------------------------------------------------------------------------

;; PARSER FOR PREFİX
;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;; '7 --> (msl-num 7)
;; '(* 5 3) --> (msl-mul (msl-num 5) (msl-num 3))
;; '(+ (* 2 3) (- 5) --> (msl-add (msl-mul (msl-num 2) (msl-num 3)) (msl-sub (msl-num 5))) --> 1

(define (parse-prefix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (msl-add (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(*) (msl-mul (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(-) (msl-sub (parse-prefix (second sl)))]
         [(**) (msl-expo (parse-prefix (second sl)) (parse-prefix (third sl)))]
         [(CheckZ) (msl-checkZ (parse-prefix (second sl)) (parse-prefix (third sl)) (parse-prefix (fourth sl)))]
         [(factorial) (msl-fact (parse-prefix (second sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

;;test
(test (parse-prefix '7) (msl-num 7))
(test (parse-prefix '(* 5 3)) (msl-mul (msl-num 5) (msl-num 3)))
(test (parse-prefix '(+ 15 13)) (msl-add (msl-num 15) (msl-num 13)))
(test (parse-prefix '(** 8 9)) (msl-expo (msl-num 8) (msl-num 9)))
(test (parse-prefix '(* 578 456)) (msl-mul (msl-num 578) (msl-num 456)))
(test (parse-prefix '(+ (* 2 3) (- 5))) (msl-add (msl-mul (msl-num 2) (msl-num 3)) (msl-sub (msl-num 5))))
(test (parse-prefix '(* (* 8 9) (- 34))) (msl-mul (msl-mul (msl-num 8) (msl-num 9)) (msl-sub (msl-num 34))))
(test (parse-prefix '(** (* 2 3) (- 2))) (msl-expo (msl-mul (msl-num 2) (msl-num 3)) (msl-sub (msl-num 2))))
(test (parse-prefix '(+ (+ 56 35) (* 5 5))) (msl-add (msl-add (msl-num 56) (msl-num 35)) (msl-mul (msl-num 5) (msl-num 5))))
(test (parse-prefix '(CheckZ 4 20 30)) (msl-checkZ (msl-num 4) (msl-num 20) (msl-num 30)))
(test (parse-prefix '(CheckZ 15 50 78)) (msl-checkZ (msl-num 15) (msl-num 50) (msl-num 78)))



;;PARSER FOR INFIX
;; parse s-expression -> msl
;; convert a quoted s expression into the equivalent msl form
;; examples
;;'2300 --> (msl-num 2300))
;;'(3 * 6) --> (msl-mul( msl-num 3) (msl-num 6))
;;'((34 ** 1) + (8 * 8)) --> (msl-add (msl-expo (msl-num 34)(msl-num 1))(msl-mul (msl-num 8)(msl-num 8)))
;;'((25 * 80) + (56 + 90)) --> (msl-mul (msl-mul (msl-num 25)(msl-num 80))(msl-add (msl-num 56)(msl-num 90)))

(define (parse-infix [s : s-expression]) : msl
  (cond
    [(s-exp-number? s) (msl-num (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (second sl))
         [(+) (msl-add (parse-infix (first sl)) (parse-infix (third sl)))]
         [(*) (msl-mul (parse-infix (first sl)) (parse-infix (third sl)))]
         [(-) (msl-sub (parse-infix (first sl)))]
         [(**) (msl-expo (parse-infix (first sl)) (parse-infix (third sl)))]
         [(CheckZ) (msl-checkZ (parse-prefix (first sl)) (parse-prefix (third sl)) (parse-prefix (fourth sl)))]
         [(factorial) (msl-fact (parse-prefix (first sl)))]
         [else (error 'parse-infix "invalid list input")]))]
    [else (error 'parse-infix "invalid input")]))


;;Tests
(test (parse-infix '8) (msl-num 8))
(test (parse-infix '346) (msl-num 346))
(test (parse-infix '2300) (msl-num 2300))
(test (parse-infix '(5 ** 3)) (msl-expo (msl-num 5) (msl-num 3)))
(test (parse-infix '(8 ** 9)) (msl-expo (msl-num 8) (msl-num 9)))
(test (parse-infix '(20 ** 15)) (msl-expo (msl-num 20) (msl-num 15)))
(test (parse-infix '(3 * 6)) (msl-mul( msl-num 3) (msl-num 6)))
(test (parse-infix '((2 * 8) * (5 * 6))) (msl-mul (msl-mul (msl-num 2)(msl-num 8))(msl-mul (msl-num 5)(msl-num 6))))
(test (parse-infix '((6 * 7) + (23 * 2))) (msl-add (msl-mul (msl-num 2)(msl-num 8))(msl-mul (msl-num 23)(msl-num 2))))
(test (parse-infix '((25 * 80) * (3 ** 3))) (msl-mul (msl-mul (msl-num 25)(msl-num 80)) (msl-expo (msl-num 3)(msl-num 3))))
(test (parse-infix '((34 ** 1) + (8 * 8))) (msl-add (msl-expo (msl-num 34)(msl-num 1))(msl-mul (msl-num 8)(msl-num 8))))
(test (parse-infix '((25 * 80) + (56 + 90))) (msl-mul (msl-mul (msl-num 25)(msl-num 80))(msl-add (msl-num 56)(msl-num 90))))
(test (parse-infix '(5 CheckZ 65 90)) (msl-checkZ (msl-num 5) (msl-num 65) (msl-num 90)))
(test (parse-infix '(100 CheckZ 20 50 )) (msl-checkZ (msl-num 100) (msl-num 20) (msl-num 50)))


;; output-reverse msl -> list of s-expression
;; output the msl as the reverse polish commands needed to evaluate it
;; examples
;; (msl-num 7) -> '(7)
;; (msl-mul (msl-num 5) (msl-num 9)) -> '(5 9 *)
;; (msl-mul (msl-sub (msl-num 3)) (msl-add (msl-num 2) (msl-num 1)))) -> '(3 + 2 1 + *)
;; (msl-add (msl-num 3) (msl-mul (msl-num 4) (msl-num 2)))) -> '(3 4 2 * +))

(define (output-reverse [expr : msl])
  (type-case msl expr
    [msl-num (n) (list (number->s-exp n))]
    [msl-add (lhs rhs) (append (append (output-reverse lhs) (output-reverse rhs))
                               (list (symbol->s-exp '+)))]
    [msl-mul (lhs rhs) (append (append (output-reverse lhs) (output-reverse rhs))
                               (list (symbol->s-exp '*)))]
    [msl-sub (lhs) (append (output-reverse lhs) (list (symbol->s-exp '+)))]
    [msl-expo (lhs rhs) (append (append (output-reverse lhs) (output-reverse rhs))
                               (list (symbol->s-exp '**)))]
    [msl-checkZ (exp1 exp2 exp3) (append (append(output-reverse exp1) (output-reverse exp2))
                                         (output-reverse exp3))]
    [msl-fact (lhs) (append (output-reverse lhs) (list (symbol->s-exp '!)))]
    ))

;;test
(test (output-reverse (msl-num 8)) (s-exp->list '(8)))
(test (output-reverse (msl-mul (msl-num 5) (msl-num 9))) (s-exp->list '(5 9 *)))
(test (output-reverse (msl-mul (msl-sub (msl-num 3)) (msl-add (msl-num 2) (msl-num 1)))) (s-exp->list '(3 + 2 1 + *)))
(test (output-reverse (msl-add (msl-num 3) (msl-mul (msl-num 4) (msl-num 2)))) (s-exp->list '(3 4 2 * +)))
(test (output-reverse (msl-mul (msl-num 5) (msl-mul (msl-num 5) (msl-num 6)))) (s-exp->list '(5 5 6 * *)))
(test (output-reverse (msl-add (msl-num 35) (msl-mul (msl-num 40) (msl-num 200)))) (s-exp->list '(35 40 200 * +)))
(test (output-reverse (msl-expo (msl-num 8) (msl-mul (msl-num 90) (msl-num 25)))) (s-exp->list '(8 90 25 * **)))
(test (output-reverse (msl-add (msl-num 678) (msl-mul (msl-num 47) (msl-num 20)))) (s-exp->list '(678 47 20 * +)))

;;------------------------------------------------------------------------------

;SUGAR LANGUAGE
;;DESUGAR

;;msl -> number
;;msl -> msl+msl
;;msl -> msl*msl
;;msl ->(msl)
;; op: [+, *, **, -]

(define-type sl
  [numS (n : number)]
  [plusS (l : sl) (r : sl)]
  [minusS (l : sl)]
  [multS (l : sl) (r : sl)])

;; sl --> msl
;; convert sl to msl
;;example
;;(plusS (numS 2) (numS 5)) -> (msl-add (msl-num 2) (msl-num 5))
;;(multS (plusS (numS 3) (numS 6)) (numS 7)) -> (msl-mul (msl-add (msl-num 3) (msl-num 6)) (msl-num 7))
;;(minusS (multS (numS 3) (numS 6))) -> (msl-sub (msl-mul (msl-num 3) (msl-num 6)))
;(minusS (multS (numS 3) (numS 6)))) (msl-sub (msl-mul (msl-num 3) (msl-num 6))
;(minusS (multS (numS 88) (numS 66)))) (msl-sub (msl-mul (msl-num 88) (msl-num 66)))
;(multS  (numS 3) (numS 6))) (msl-mul  (msl-num 3) (msl-num 6))
;(minusS (multS (numS 6) (numS 90)))) (msl-sub (msl-mul (msl-num 6) (msl-num 90)))

(define (desugar [as : sl]) : msl
  (type-case sl as
    [numS (n) (msl-num n)]
    [plusS (l r) (msl-add (desugar l)
                        (desugar r))]
    [multS (l r) (msl-mul (desugar l)
                        (desugar r))]
    [minusS (l) (msl-sub (desugar l))]
))

;;Test
(test (desugar (plusS (numS 2) (numS 5))) (msl-add (msl-num 2) (msl-num 5)))
(test (desugar (plusS (numS 9) (numS 2))) (msl-add (msl-num 9) (msl-num 2)))
(test (desugar (plusS (numS 15) (numS 12))) (msl-add (msl-num 15) (msl-num 12)))
(test (desugar (multS (plusS (numS 3) (numS 6)) (numS 7))) (msl-mul (msl-add (msl-num 3) (msl-num 6)) (msl-num 7)))
(test (desugar (minusS (multS (numS 3) (numS 6)))) (msl-sub (msl-mul (msl-num 3) (msl-num 6))))
(test (desugar (minusS (multS (numS 88) (numS 66)))) (msl-sub (msl-mul (msl-num 88) (msl-num 66))))
(test (desugar (multS  (numS 3) (numS 6))) (msl-mul  (msl-num 3) (msl-num 6)))
(test (desugar (minusS (multS (numS 6) (numS 90)))) (msl-sub (msl-mul (msl-num 6) (msl-num 90))))
(test (desugar (minusS (multS (numS 3) (numS 6)))) (msl-sub (msl-mul (msl-num 3) (msl-num 6))))
(test (desugar (plusS (multS (numS 67) (numS 33)) (numS 89))) (msl-add (msl-mul (msl-num 67) (msl-num 33)) (msl-num 89)))
(test (desugar (minusS (multS (numS 78) (numS 78)))) (msl-sub (msl-mul (msl-num 78) (msl-num 78))))

;;-----------------------------------------------------------------

;; Grammar for ExprC
;; ExprC -> number
;; ExprC -> symbol
;;appC -> <msl> <msl>


(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [factC (l : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define double (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
(define quadruple (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x)))))
(define const (fdC 'const '_ (numC 99)))

(define funcList (list double quadruple const))

;;INTERP
;;ExprC * (listof FunDefC) -> Number
;;numC 10) funcList) ->  10
;;numC 15) funcList) ->  15
;;numC 7) funcList) ->  7
;;(plusC (numC 8) (numC 5)) funcList) ->  13
;;(plusC (numC 4) (numC 6)) funcList) ->  10
;;(plusC (numC 6) (numC 6)) funcList) ->  12
;;(factC (num 4) funcList) -> 24
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
  [numC (n) n]
  [idC (_) (error 'interp "shouldn't get here")]
  [appC (f a) (local ([define fd (get-fundef f fds)])
              (interp (subst a
                             (fdC-arg fd)
                             (fdC-body fd))
                      fds))]
  [plusC (l r) (+ (interp l fds) (interp r fds))]
  [multC (l r) (* (interp l fds) (interp r fds))]
  [factC (l) (factorial (interp l fds))]
  ))

;;
(test (interp (numC 7) funcList)  7)
(test (interp (numC 8) funcList)  8)
(test (interp (numC 10) funcList)  10)
(test (interp (plusC (numC 3) (numC 4)) funcList)  7)
(test (interp (plusC (numC 8) (numC 5)) funcList)  13)
(test (interp (plusC (numC 9) (numC 2)) funcList)  11)
(test (interp (multC (numC 4) (numC 5)) funcList)  20)
(test (interp (multC (numC 4) (numC 5)) funcList)  20)
(test (interp (multC (numC 8) (numC 8)) funcList)  64)
(test (interp (multC (numC 20) (numC 3)) funcList)  60)
(test (interp (factC (numC 3)) funcList)  6)


;;SUBST
;;ExprC * symbol * ExprC -> ExprC
;;(plusC (numC 9)(numC 9)) 'x (idC 'y)) -> (numC 18)
;;(plusC (numC 5)(numC 10)) 'x (idC 'x)) -> (numC 15)
;;(multC (numC 4)(numC 3)) 'y (numC 5)) -> (numC 5)
;;(multC (numC 3)(numC 2)) 'x (idC 'y)) -> (numC 6)
;;(numC 4) 'y (multC (numC 4)(numC 3)) ) -> (numC 12)
;;(idC 'y) 'x (multC (numC 3)(numC 2))) -> (numC 6)

(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
  [numC ( n ) in]
  [idC (s) (cond
             [(symbol=? s for) what]
             [else in])]
  [appC (f a) (appC f (subst what for a))]
  [plusC (l r) (plusC (subst what for l)
                      (subst what for r))]
  [multC (l r) (multC (subst what for l)
                      (subst what for r))]
  [factC (l) (factC (subst what for l))]))

;;test

(test (subst (numC 4) 'x (numC 5)) (numC 5))
(test (subst (numC 8) 'x (idC 'x))  (numC 8))
(test (subst (plusC (numC 9)(numC 9)) 'x (idC 'y)) (numC 18))
(test (subst (plusC (numC 5)(numC 10)) 'x (idC 'x)) (numC 15))
(test (subst (multC (numC 4)(numC 3)) 'y (numC 5)) (numC 5))
(test (subst (multC (numC 3)(numC 2)) 'x (idC 'y)) (numC 6))
(test (subst (numC 4) 'y (multC (numC 4)(numC 3)) ) (numC 12))
(test (subst (idC 'y) 'x (multC (numC 3)(numC 2))) (numC 6))

;;Get-Fundef
;;symbol * (listof FunDefC) -> FunDefC


(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


;;Factorial Function

(define (factorial n)
  (cond
    ((= n 0) 1)
    (else
      (* n (factorial (- n 1))))))

(test (factorial 3) 6)
(test (factorial 4) 24)
(test (factorial 5) 120)

;;-------------------------------------------------------------------

;; λ-expression grammar
;; λ-exp -> v
;; λ-exp -> (λ-exp λ-exp)
;; λ-exp -> (λ v λ-exp)
(define-type λ-exp
  (λ-sym (v : symbol))
  (λ-app (l : λ-exp)(r : λ-exp))
  (λ-def (v : symbol)(p : λ-exp))
  )

;;Parseλ
;;s-exp -> λ-exp
(define (parseλ (expr : s-expression)) : λ-exp
  (cond
    [(s-exp-symbol? expr)(λ-sym (s-exp->symbol expr))]
    [(s-exp-list? expr)
     (let ([expr-list (s-exp->list expr)])
       (cond
         [(= 2 (length expr-list))
          (λ-app (parseλ (first expr-list))(parseλ (second expr-list)))]
         [(= 3 (length expr-list))
          (if (and (symbol=? 'λ (s-exp->symbol (first expr-list)))
                   (s-exp-symbol? (second expr-list)))
              (λ-def (s-exp->symbol(second expr-list))
                     (parseλ (third expr-list)))
              (error 'parseλ "Not valid λ-definition")
              )]
         [else (error 'parseλ "Not valid length λ-exp")]
         ))]
    [else (error 'parseλ "Not valid λ-exp")]
    ))

         
;; set-union : (listof symbol) (listof symbol) -> (listof symbol)
;; To find the union of two sets.
;; (list 'x) (list 'x)) (list 'x)
;; (list 'x)(list 'x 'y)) (list 'x 'y)
;; (list 'x 'y) (list 'x 'y)) (list 'x 'y)
(define (union (s1 : (listof symbol)) (s2 : (listof symbol))) : (listof symbol)
  (foldr (lambda (x y)
           (if (member x y)
               y
               (cons x y))) 
         empty
         (append s1 s2)))

;; Tests:
(test (union empty empty) empty)
(test (union empty (list 'x)) (list 'x))
(test (union (list 'x)(list 'x 'y)) (list 'x 'y))
(test (union (list 'x) (list 'x)) (list 'x))
(test (union (list 'x 'y) (list 'x 'y) ) (list 'x 'y))

;; set-difference : (listof symbol) (listof symbol) -> (listof symbol)
;; (list 'x 'y)(list 'x)) -> (list 'y)
;; (list 'x)(list 'x 'y)) -> (list 'y)
;; (list 'x) empty) -> (list 'x)
(define (difference (sl : (listof symbol))  (sr : (listof symbol))) : (listof symbol)
  (filter (lambda (x)
            (not (member x sr)))
          sl))

;; Tests:
(test (difference empty (list 'x)) empty)
(test (difference (list 'x) empty) (list 'x))
(test (difference (list 'x)(list 'x 'y)) (list 'y))
(test (difference (list 'x 'y)(list 'x))(list 'y))

;; free-identifier : λ-exp -> (listof symbol)
;; (parseλ '((λ x y)(λ y z)))) -> (list 'y 'z)
;; (parseλ '((λ f y)(λ z z)))) -> (list 'y))
;; (parseλ '((λ x y)(λ y y)))) (list 'y)
;; (parseλ '(λ x (λ y (λ y x))))) empty)
(define (free-identifier (le : λ-exp)) : (listof symbol)
  (type-case λ-exp le
    (λ-sym (v) (list v))
    (λ-app (l r)(union 
                 (free-identifier l)
                 (free-identifier r)))
    (λ-def (v p)(difference (free-identifier p)
                                (list v)))
    ))

;; Tests:
(test (free-identifier (parseλ (symbol->s-exp 'x))) (list 'x))
(test (free-identifier (parseλ '(λ x x))) empty)
(test (free-identifier (parseλ '(λ x y))) (list 'y))
(test (free-identifier (parseλ '((λ x y)(λ y y)))) (list 'y))
(test (free-identifier (parseλ '(λ x (λ y (λ y x))))) empty)
(test (free-identifier (parseλ '(λ x (λ (λ x y) z)))) (list 'z ))