
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
  (if (> low high)
      (list )
      (append (list low) (sequence (+ low stride) high stride)))
  )
  
(define (string-append-map xs suffix) 
  (map (lambda (x) (string-append x suffix)) xs) 
  )

(define (list-nth-mod xs n) 
  (if (< n 0)
      (error "list-nth-mod: negative-number")
      (if (empty? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs))))
          )
  ))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (define (f x) 
    (cons (if (= 0 (remainder x 5)) (- x) x)
          (lambda () (f (+ x 1)))))
  (f 1))

(define (cat-then-dog) 
  (define (f x)
    (cons (if (= 0 (remainder x 2)) "dog.jpg" "cat.jpg")
          (lambda () (f (+ x 1)))))
  (f 1))

(define (stream-add-zero s)
  (define (f x)
    (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))
  (lambda () (f s)))

(define (cycle-lists xs ys) 
  (define (f n)  
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
          (lambda () (f (+ n 1)))))
  (lambda () (f 0)))

(define (vector-assoc v vec) 
  (define (f x)
    (if (>= x (vector-length vec)) 
        #f
        (if (eq? v (car (vector-ref vec x)))
        (vector-ref vec x)
        (f (+ x 1)))))
  (f 0))
