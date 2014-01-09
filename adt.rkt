#lang scheme


;;6.1
(define (make-punt x y) (list x y))

(define (x punt)
  (car punt))
(define (y punt)
  (cadr punt))

(define (make-segment start einde)
  (list start einde))

(define (start-punt segment)
  (car segment)
  )
(define (eind-punt segment)
  (cadr segment))

(define (middelpunt segment)
  (make-punt (/ (+ (x (start-punt segment)) 
                   (x (eind-punt segment)))
                2)
             (/ (+ (y (start-punt segment)) 
                   (y (eind-punt segment)))
                2)))
  
;;6.2.1
(define (make-vector-l . coo)
  coo)
(define (dimension-l v-l)
  (length v-l))
(define (coo-l v-l n)
  (list-ref v-l n))

(define a (make-vector-l 1 2 3))
(dimension-l a)
(coo-l a 1)
  
;;6.2.2
(define (make-vector-v . coo)
  (list->vector coo))

(define (dimension-v v-l)
  (vector-length v-l))

(define (coo-v v-l n)
  (vector-ref v-l n))

(define b (make-vector-v 1 2 3))
(dimension-v b)
(coo-v b 1)

;;6.2.5
(define (vector-som v1 v2)
  (map + (vector->list v1) (vector->list v2)))

(define v1-v (make-vector-v 1 2 3))
(define v2-v (make-vector-v 4 5 6))
(vector-som v1-v v2-v)

(define (vector-scalair-product v fac)
  (map (lambda (x) (* x fac)) (vector->list v)))

(vector-scalair-product v1-v 2)

(define (vector-verschil v1 v2)
  (vector-som v1 (list->vector (vector-scalair-product v2 -1))))

(define vv1-v (make-vector-v 4 5 6))
(define vv2-v (make-vector-v 1 2 3))
(vector-verschil vv1-v vv2-v)

(define (vector-inproduct v1 v2)
  (apply +
         (map * (vector->list v1) (vector->list v2))))

(define vi1-v (make-vector-v 4 5 6))
(define vi2-v (make-vector-v 1 2 3))
(vector-inproduct vi1-v vi2-v)

(define (make-punt x y) (make-vector-v x y))
(define (x punt) (coo-v punt 0))
(define (y punt) (coo-v punt 1))