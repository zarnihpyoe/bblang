#lang plai-typed
#| 
Testing the interpreter for
Top-level functions
|# 
(require "first-class-funcs.zphyo.rkt")

;; basic ops tests
;; ===============

;; + * -
(test (run '5) (numV 5))
(test (run '(+ 2 3)) (numV 5))
(test (run '(* 2 3)) (numV 6))
(test (run '(- 5 3)) (numV 2))
;; if0
(test (run '(if0 0 1 2)) (numV 1))
(test (run '(if0 1 1 2)) (numV 2))
(test (run '(if0 (- (- 2 3) -1) (* 2 2) (+ 1 5))) (numV 4))
;; nested if0
(test (run '(if0 (if0 1 1 0)
                 (if0 0 4 5)
                 2)) (numV 4))

;; functions
;; =========

;; function as a return value
(test (run '(fun (x) 0))
      (closV (list 'x) (numC 0) mt-env))
;; function applications
(test (run '((fun (x) 0) 3)) (numV 0))
(test (run '((fun (x) (+ x 1)) 3)) (numV 4))
(test (run '((fun (x y) (- x y)) 3 5)) (numV -2))
;; function applications with if0
(test (run '((fun (x y) (if0 x y (+ y 1))) 0 2))
      (numV 2))
(test (run '((fun (x y) (if0 x y (+ y 1))) 1 2))
      (numV 3))

;; nested functions
;; ================

;; function as a return value
(test (run '((fun (x y)
                  (fun (z) (+ x y)) ) 3 5) )
      (closV (list 'z)
             (plusC (idC 'x) (idC 'y))
             (list (bind 'y (numV 5)) (bind 'x (numV 3))) ))
;; function applications
(test (run '(((fun (x)
                  (fun (y) (+ x y))) 3) 5) )
      (numV 8))

(test (run '((((fun (x)
                  (fun (y z)
                       (fun (a) (+ a (+ (+ x y) z))))) 3) 5 4) 2) )
      (numV 14))
;; function with same parameter names as inner function
(test (run '(((fun (y)
                  (fun (y) (+ y y))) 1) 2) )
      (numV 4))

;; with
;; ====

;; simeple with
(test (run '(with ( (x 5) ) x)) (numV 5))
;; with + named function definition
(test (run '(with ((f (fun (x) (* x 2)))) (f 5))) (numV 10))
(test (run '(with ( (x 5)
                    (y 6)
                    (f (fun (a b) (- a b))))
                  (f x y))) (numV -1))


;; exceptions
(test/exn (run '(with ((x 5)) y)) "unbound")
(test/exn (run '((fun (x y x) 3) 4 4 4)) "multiple")
(test/exn (run '(3 4)) "type")
(test/exn (run '(if0 (fun (x) 5) 3 4)) "type")

;; exceptions in bindings different lengths of arrays
(test/exn (add-bindings (list 'a 'b) (list (numV 1)) mt-env) "lengths")
(test/exn (add-bindings (list 'a) (list (numV 1) (numV 2)) mt-env) "lengths")
