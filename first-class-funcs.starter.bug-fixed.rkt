;; BUG FIX 2018 Jan 23 21:05:28
;;
;; in line 28 below, function "multiple-names-error"
;; the occurence of "string-list:  should be
;; "(to-string string-list)"
;; in my solution-code, I made the call to to-string when I called
;; "multiple-names-error", which is why my tests passed...but the
;; current fixed function is more sensible.

#lang plai-typed
#| 
Language, parsing, desugaring, and interpreting for:
First-class functions
(multiple arguments)
|#

;; General utilities
;; -----------------

;; useful placeholder while developing
;; will match any type, for the purpose of compiling...
(define (undefined) (error 'undefined "") )

;; True if the two lists have *different* lengths
;; different-length? (listof 'a) -> (listof 'a) -> boolean
  (define (different-length? lst1 lst2) : boolean
  (not (= (length lst1) (length lst2))))

;; complaint when string-list has duplicates
;; multiple-names-error : symbol -> (listof string) -> void
(define (multiple-names-error tag string-list)
   ;; OLD, BUG
   ;; (error tag (string-append "name occurs multiple times: " string-list)))
  (error tag (string-append "name occurs multiple times: " (to-string string-list))))

;; complaint when lst lengths don't match
;; (ugh, string-append wants exactly two arguments)
;; length-mismatch-error : symbol -> (listof string) -> (listof string) -> void
(define (length-mismatch-error tag lst1 lst2)
  (error tag
         (string-append "string lengths don't match "
                        (string-append (to-string lst1) (to-string lst2))
                        )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Definition, Parsing, (De-)Sugaring
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Types
;; ------
;; the core language     
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (args : (listof ExprC))]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [lamC (args : (listof symbol)) (body : ExprC)]
  )

;; Sugared Syntax
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (args : (listof ExprS))]
  [if0S (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (args : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof Def)) (body : ExprS)]
  )

;; Definitions, as used in a with-form
(define-type Def
  [defS (name : symbol) (val : ExprS)])


;; desugar : ExprS -> ExprC
(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [bminusS (l r) (plusC (desugar l)
                        (multC (numC -1)
                               (desugar r)))]
    [idS (i)  (idC i)]
    [lamS (args body) (lamC args (desugar body))]
    [appS (f args)  (appC (desugar f)
                          (map desugar args))]
    [if0S (c t e) (if0C (desugar c)
                        (desugar t)
                        (desugar e))]
    [withS (bindings body) (undefined)]
    ))


;; Parsing
;; --------
     
;; parse : s-expression -> ExprS
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl))
              ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(if0) (if0S (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (lamS (map s-exp->symbol (s-exp->list (second sl))) 
                                (parse (third sl)))]
                [(with) (withS (map (lambda (b) 
                                      (let ([bl (s-exp->list b)])
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl)))]
                [else ;; must be a function call using function name
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl)))])]
             [(s-exp-list? (first sl)) ;; function call with complex expression in function position
              (appS (parse (first sl))
                    (map parse (rest sl)))]
             [(s-exp-number? (first sl))
              ;; type violation: using number as function (but fits grammar)
              (appS (parse (first sl)) (map parse (rest sl)))]
             [else (error 'parse "expected symbol or list after parenthesis")]))]
    [else (error 'parse "unexpected input format")]))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreting
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Types
;; -----

(define-type Value
  [numV (n : number)]
  [closV (params : (listof symbol)) (body : ExprC) (env : Env)])

;; Environments
(define-type Binding
  [bind (name : symbol) (val : Value)])
(define-type-alias Env (listof Binding))


;; Local Utilities
;; ---------------

;; mt-env : Env
(define mt-env empty)

;; extend-env : Binding -> Env -> Env
(define extend-env cons)

;; return first value bound to id in env, or raise error if id is not found
;; lookup : symbol  -> Env -> Value
(define (lookup [id : symbol] [env : Env]) : Value
  (cond [(empty? env) (error 'lookup (string-append "unbound identifier " (to-string id)))]
        [(cons? env) (if (symbol=? id (bind-name (first env)))
                         (bind-val (first env))
                         (lookup id (rest env)))]))

(test (lookup 'a (list (bind 'a (numV 1))))
      (numV 1))
(test (lookup 'a (list (bind 'b (numV 2))
                       (bind 'a (numV 1))))
      (numV 1))

;; error unless names and vals have the same length
;; add-bindings : (listof symbol  -> listof Value -> Env -> Env
(define (add-bindings [names : (listof symbol)] [vals : (listof Value)] [env : Env]) : Env
  (cond [(empty? names) env]
        [(cons? names) (if (different-length? names vals)
                           (length-mismatch-error 'binding names vals)
                           (add-bindings (rest names) (rest vals)
                                         (extend-env (bind (first names) (first vals)) env)))]))
(test (add-bindings (list 'a 'b) (list (numV 1) (numV 2)) mt-env)
      (list (bind 'b (numV 2))
            (bind 'a (numV 1))))
(test (add-bindings (list 'a 'b) (list (numV 1) (numV 2)) (list (bind 'c (numV 3))))
      (list (bind 'b (numV 2))
            (bind 'a (numV 1))
            (bind 'c (numV 3))))
(test/exn (add-bindings (list 'a 'b) (list (numV 1)) mt-env) "lengths")
(test/exn (add-bindings (list 'a) (list (numV 1) (numV 2)) mt-env) "lengths")

;; operators on numVs
;; num+ : Value -> Value -> Value
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "type error: one argument was not a number")]))

;; num* : Value -> Value -> Value
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "type error: one argument was not a number")]))

;; num0? : Value -> boolean
(define (num0? [v : Value]) : boolean
  (if (numV? v) 
      (zero? (numV-n v))
      (error 'num0? "type error: argument was not a number")))

;; helpers to check for duplicate names
(define (multiples? lst)
  (cond [(empty? lst) false]
        [(cons? lst) (or (member (first lst) (rest lst))
                         (multiples? (rest lst)))]))

;; Interpreter
;; ------------------------------------------------------------------------

;; interp : ExprC -> Env -> Value
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
             [numC (n) (numV n)]
             [plusC (l r) (num+ (interp l env) (interp r env))]
             [multC (l r) (num* (interp l env) (interp r env))]
             [idC (i) (lookup i env)]
             [if0C (c t e)
                   (cond [(num0? (interp c env)) (interp t env)]
                         [else (interp e env)])]
             [lamC (params body) (closV params body env)]
             [appC (f args) (if (numC? f)
                                (error 'interp "type error: unexpected number")
                                (apply f args env))]
             ))

;; apply : ExprC -> (listof ExprC) -> Env -> Value
(define (apply  [f : ExprC] [args : (listof ExprC) ] [env : Env]) : Value
  (let* ( (fv (interp f env))
          (params (closV-params fv))
          (body (closV-body fv))
          (envv (closV-env fv)) )
    (if (multiples? (closV-params fv))
        (multiple-names-error 'apply (closV-params fv))
        (interp body
                (add-bindings params
                              (map (lambda (arg) (interp arg env)) args)
                              envv)))))


;; ------------------------------------------------------------------------
;; Running;  testing
;; --------------------------------

;; evaluates a program starting with a pre-populated environment
;; (this can be helpful in testing)

;; run/env : s-expression ->  Env -> Value
(define (run/env sexp env)
  (interp (desugar (parse sexp)) env))

;; evaluates a program in the empty environment

;; run : s-expression -> Value
(define (run sexp)
  (run/env sexp mt-env))


;; basic ops tests
(test (run '5) (numV 5))
(test (run '(+ 2 3)) (numV 5))
(test (run '(* 2 3)) (numV 6))
(test (run '(- 5 3)) (numV 2))

(test (run '(if0 0 1 2)) (numV 1))
(test (run '(if0 1 1 2)) (numV 2))
(test (run '(if0 (- (- 2 3) -1) (* 2 2) (+ 1 5))) (numV 4))

;; testing basic functions
(test (desugar (parse '(+ 1 2))) (plusC (numC 1) (numC 2)))
(test (desugar (parse '(fun (x) 0))) (lamC (list 'x)
                                           (numC 0)))
(test (desugar (parse '(fun (x y z) (+ 5 4)))) (lamC
                                            (list 'x 'y 'z)
                                            (plusC (numC 5) (numC 4))))
(test (desugar (parse '(fun (x y) (+ x y)))) (lamC
                                            (list 'x 'y)
                                            (plusC (idC 'x) (idC 'y))))
(test (desugar (parse '(((fun (x)
                  (fun (y) (+ x y))) 3) 4)))
      (appC (appC (lamC (list 'x)
                        (lamC (list 'y)
                              (plusC (idC 'x) (idC 'y))))
                  (list (numC 3)))
            (list (numC 4))))

(test (run '(fun (x) 0)) (closV (list 'x)
                                (numC 0)
                                mt-env))

;;(test (parse '((fun (x) 0) 3)) (appS (lamS (list 'x) (numS 0))
;;                                     (list (numS 3))))
(test (desugar (parse '((fun (x) 0) 3))) (appC (lamC (list 'x) (numC 0))
                                     (list (numC 3))))

(test (run '((fun (x) 0) 3)) (numV 0))
(test (run '((fun (x) (+ x 1)) 3)) (numV 4))
(test (run '((fun (x y) (+ x y)) 3 5)) (numV 8))


;; testing same parameter name in nested funcs
(test (run '(((fun (y)
                  (fun (y) (+ y y))) 3) 4) )
      (numV 8))

(test (run '(((fun (x)
                  (fun (y) (+ x y))) 3) 5) )
      (numV 8))

(test (run '((((fun (x)
                  (fun (y z)
                       (fun (a) (+ a (+ (+ x y) z))))) 3) 5 4) 2) )
      (numV 14))



;; testing nested func return closV
(test (run '((fun (x y)
                  (fun (z) (+ x y)) ) 3 5) )
      (closV (list 'z)
             (plusC (idC 'x) (idC 'y))
             (list (bind 'y (numV 5)) (bind 'x (numV 3))) ))


;; testing simple with
;; (test (parse '(with ( (x 5) ) x)) (numV 5))

;; Some tests
;;(test (run '(+ (* 5 (+ 7 3)) 4)) (numV 54))
;;(test (run '(if0 (+ 2 2) 6 8)) (numV 8))
;;; (test (run '(with ((f (fun (x) (* x 2)))) (f 5))) (numV 10))
;;; (test/exn (run '(with ((x 5)) y)) "unbound")
(test/exn (run '((fun (x y x) 3) 4 4 4)) "multiple")
(test/exn (run '(3 4)) "type") 
(test/exn (run '(if0 (fun (x) 5) 3 4)) "type")
