#lang scheme
(define (display-n x y)
  (cond ((> y 0) 
         (display x)
         (display-n x (- y 1)))
        ))
         
  (define f (lambda (x) (* x 10)))
  (define g (lambda (y) (+ y 5)))
  (define mysterious (lambda (f g) (lambda (x) (f (g x)))))
  (define fg (mysterious f g))
  
  (define (next a)
    (+ a 1))
  (define (term a)  a)
 
  (define (sum term a next b)
    (if (> a b) 0 
        (+ (term a) (sum term (next a) next b))))
  (define (mod-3 a b) (remainder ((- a b) 3)))
  
  (define (simp-int f a b n)
    (define (next n) (- 1 n))
    (define (sum term a next b)
      (let ((h (/ (- b a) n))
            (fact (cond 
                    [(= 0 n) 1]
                    [(= n n-orig) 1]
                    [(even? n) 4]
                    [else 2]))
        
        (define (term a) (f (+ a ( * h (- a b))))
        (if (> a b) 0 
            (*
             (/ h 3)
             (+ (term a) (sum term (next n) next b)))))))
      (sum term 0 next b)))
  
  (define (factor a) a)
  (define (product factor a next b)
    (if (> a b) 1
        (* (term a) (product factor (next a) next b))))
    
  (define (product-iter factor a next b)
    (define (product-iter-helper result factor a next b)
      (if (> a b) result
          (product-iter-helper (* (factor a) result) factor (next a) next b)))
    (product-iter-helper 1 factor a next b))