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
(define (change e1 e2 l)
  (if (null? l)
      '()
      (if (eq? e1 (car l))
          (cons e2 (change e1 e2 (cdr l)))
          (cons (car l) (change e1 e2 (cdr l)))
          )))

(change 1 999 '(1 2 1 3 1 4 1 5 1))
(change 1 999 '(2 3 4 5 6 7))
(change 1 999 '(1))
(change 1 999 '())

;;5.11
(define  (my-equal l1 l2)
  (if (null? l1)
      (if (null? l2)
          #t
          #f)
      (if (null? l2)
          (if (null? l1)
              #t
              #f)
          (if (eq? (car l1) (car l2))
              (my-equal (cdr l1) (cdr l2))
              #f))))

(my-equal '(1 2 3) '(1 2 3))
(my-equal '(1 2 3) '(4 5 6))
(my-equal '(1 2 3) '(1 2))
(my-equal '(1 2 3) '())
(my-equal '() '(1 2 3))

;;5.12
(define (sum-lists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (cons (+ (car l1) (car l2)) (sum-lists (cdr l1) (cdr l2))))))

(sum-lists '(1 2 3 4) '(5 6 7))

(define (sum-lists-iter l1 l2)
  (define (iter ls1 ls2 res)
    (cond ((null? ls1) (append res ls2))
          ((null? ls2) (append res ls1))
          (else (iter (cdr ls1) (cdr ls2) (append res (list (+ (car ls1) (car ls2)))))))
    )
  (iter l1 l2 '()))
(sum-lists-iter '(1 2 3 4) '(5 6 7))

;;5.13
(define (map a-function a-list)
  (if (null? a-list)
      '()
      (cons (a-function (car a-list))
            (map a-function (cdr a-list)))))
 
(define (combine-over-list op unity a-list)
  (if (null? a-list)
      unity
      (op (car a-list)
          (combine-over-list op unity (cdr a-list)))))

(define (sq x) (* x x))

(define (sum-of-sq l)
  (combine-over-list + 0 (map sq l)))

(sum-of-sq '(1 2 3 4 5))

(define (my-and a b)
  (and a b))

(define (enkel-even l)
  (combine-over-list my-and #t (map even? l)))

(enkel-even '( 1 2 3 4 5))
(enkel-even '( 2 4 6))

;;5.14
  