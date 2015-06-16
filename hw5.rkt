;; Jorge Conde V00723209
;; Programming Languages, Homework 5 version 1.1
#lang racket


(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
  (if (apair? lst)
      (append (list (apair-e1 lst)) (mupllist->racketlist (apair-e2 lst)) )
      (list )
  ))
(define (racketlist->mupllist lst)
  (if (list? lst)
      (if (empty? lst)
          (aunit)
          (if (list? (car lst))
                     (apair (racketlist->mupllist (car lst)) (racketlist->mupllist (cdr lst))) 
                     (apair (car lst) (racketlist->mupllist (cdr lst)))
          )
       )
      (aunit)
  ))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))   
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")
               ))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1) 
             (apair-e1 v1) 
             (error "MUPL fst applied to a non pair")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1) 
             (apair-e2 v1) 
             (error "MUPL snd applied to a non pair")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(aunit? e) e]
        [(isaunit? e) 
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1) 
             (int 0)
             )]
        [(fun? e)
         (closure env e)]
        [(call? e) 
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([c1 (closure-fun v1)]
                      [c2 (closure-env v1)]
                      [cname (cons (fun-nameopt c1) v1)]
                      [cform (cons (fun-formal c1) v2)])
                 (eval-under-env
                  (fun-body c1)
                  (if (eq? (car cname) #f)
                      (cons cform c2)
                      (cons cform (cons cname c2)))))
               (error "MUPL applied to non-closure")))]
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))]
        [(closure? e) e]
        
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) 
  (if (aunit? e1) e2 e3))

(define (mlet* lstlst e2) 
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst))
            (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem D

(define mupl-map 
  (fun #f "x" (fun "loop" "xs" 
                   (ifgreater (isaunit (var "xs")) (int 0) 
                              (aunit) 
                              (apair 
                               (call (var "x") (fst (var "xs")))
                               (call (var "loop") (snd (var "xs"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i" 
             (call (var "map") 
                   (fun #f "x" (add (var "x") (var "i")))))))
