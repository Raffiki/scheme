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
;;5.15.1
(define (take-n l n)
  (if (= 0 n) '()
  (cons (car l) (take-n (cdr l) (- n 1)))))

(take-n '(1 2 3 4 5) 2)

(define (take-n-n ls1 ls2 n)
  (append (take-n ls1 n) (take-n ls2 n)))

(take-n-n '(1 2 3 4 5) '(6 7 8 9) 2)

(define (tail-n l n)
  (if (= 0 n) l
      (tail-n (cdr l) (- n 1))))

(tail-n '(1 2 3 4 5) 2)

(define (merge-n ls1 ls2 n)
  (cond ((< (length ls1) n) (append ls1 ls2))
        ((< (length ls2) n) (append ls1 ls2))
        (else (append (take-n-n ls1 ls2 n) (merge-n (tail-n ls1 n) (tail-n ls2 n) n)))))
               
(merge-n '(1 2 3 4 5) '(6 7 8 9) 2)
(merge-n '(1 2 3 4 5) '(6 7 8 9) 3)
(merge-n '(1 2) '(3 4 5 6) 4)

;;5.15.2
(define (merge-n-iter ls1 ls2 n)
  (define (iter l1 l2 res n)
    (cond ((< (length l1) n) (append res l1 l2 ))
          ((< (length l2) n) (append res l1 l2 ))
          (else (iter (tail-n l1 n) (tail-n l2 n) (append (take-n-n l1 l2 n) res) n))))
  (iter ls1 ls2 '() n))
               
(merge-n-iter '(1 2 3 4 5) '(6 7 8 9) 2)
(merge-n-iter '(1 2 3 4 5) '(6 7 8 9) 3)
(merge-n-iter '(1 2) '(3 4 5 6) 4)
  
;;5.15.3
(define (smaller-than-n? ls n)
  (cond ((null? ls) #f)
        ((< (length (car ls)) n) #t)
        (else (smaller-than-n? (cdr ls) n))))

(smaller-than-n? '((a b c d e f)
                   (g)
                   (r s t u v w))
                 3)
(smaller-than-n? '((a b c d e f)
                   (g h i j k)
                   (l m n o p q)
                   (r s t u v w))
                 3)

(define (append-lists ls)
  (if (null? ls) '()
      (append (car ls) (append-lists (cdr ls)))))

(append-lists '((a b c d e f)
                   (g h i j k)
                   (l m n o p q)
                   (r s t u v w)))

(define (tails-n lsts n)
  (if (null? lsts) '()
      (cons (tail-n (car lsts) n) (cdr lsts))))
   
(tails-n '((a b c d e f)
                   (g h i j k)
                   (l m n o p q)
                   (r s t u v w)) 3)

(define (takes-n lsts n)
  (if (null? lsts) '()
      (append (take-n (car lsts) n) (takes-n (cdr lsts) n))))

(takes-n '((a b c d e f)
                   (g h i j k)
                   (l m n o p q)
                   (r s t u v w)) 3)

(define (super-merge-n lsts n)
  (if (smaller-than-n? lsts n) (append-lists lsts)
      (append (takes-n lsts n) (super-merge-n (tails-n lsts n) n))))
      
 (super-merge-n '((a b c d e f)
                   (g h i j k)
                   (l m n o p q)
                   (r s t u v w)) 3)
  
 ;;5.24
 ;;5.24.1
 (define (get-korting aankoop kortingen)
   (cond ((null? kortingen) 0)
         ((eq? (car aankoop) (car (car kortingen))) (cadr (car kortingen)))
         (else (get-korting aankoop (cdr kortingen)))))
 
  (get-korting '(jas 100)
          '((jas 50) (kleed 50) (rok 30) (trui 20)))
  
  (define (my-percent bedrag korting)
    (- bedrag (* bedrag (/ korting 100))))
  
  (my-percent 100 25)
  
 (define (get-prijs aankopen kortingen)
   (if (null? aankopen) '()
       (cons (my-percent (cadr (car aankopen)) (get-korting (car aankopen) kortingen)) (get-prijs (cdr aankopen) kortingen)))) 
 
 (get-prijs '((jas 100) (trui 25) (rok 70) (t-shirt 20))
          '((jas 50) (kleed 50) (rok 30) (trui 20)))
 
 (define (totaal aankoop korting)
   (combine-over-list + 0 (get-prijs aankoop korting)))
 
(totaal '((jas 100) (trui 25) (rok 70) (t-shirt 20))
          '((jas 50) (kleed 50) (rok 30) (trui 20)))
   
 
 
  
  
  
  
  
  
  
  
  
  