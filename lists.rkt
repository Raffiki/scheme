#lang scheme


;;http://soft.vub.ac.be/SCPI/Lijsten.html

(define (my-cons x y)
  (lambda (m) (m x y)))
 
(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))
 
(define a (my-cons 1 2))
 

;;5.6
(define (add-to-end x lst)
  (if (null? lst)
      (cons x '())
      (cons (car lst)
       (add-to-end x (cdr lst)))))

(add-to-end 999 '(1 2 3 4 5))

;;5.8
(define (reverse l) 
   (if (null? l) 
      '()
      (append (reverse (cdr l)) (list (car l)))))
    
(reverse '( 1 2 3 4 5))


(define (reverse-iter lst)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst) (cons (car lst) res))))
  (iter lst '()))

(reverse-iter '(1 2 3 4 5))
                
(define (add-to-end-iter x lst)
  (reverse-iter (cons x (reverse-iter lst))))

(add-to-end-iter 999 '(1 2 3 4 5))

;;5.7
(define (append-iter lst1 lst2)
  (define (iter lst res)
    (if (null? lst) res
        (iter (cdr lst) (cons (car lst) res))))
  (iter (reverse-iter lst1) lst2))

(append-iter '(1 2 3) '(4 5))

;;5.9
(define (last lst)
  (define (lest prev lst) 
    (if (null? lst)
        prev
        (lest (car lst) (cdr lst))))
  (lest #f lst))

(last '(1 2 3))
(last '(1))
(last '())


;;5.10