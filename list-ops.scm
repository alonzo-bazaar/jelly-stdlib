(define (reverse lst)
  (do ((iter lst (cdr iter))
        (acc nil (cons (car iter) acc)))
    ((null? iter) acc)))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(define (reverse-range start end)
  (let ((acc nil))
    (do ((i start (+ i 1)))
        ((>= i end) acc)
      (set! acc (cons i acc)))))

(define (range start end)
  (reverse (reverse-range start end)))

(define (car c) (call c "getCar"))
(define (cdr c) (call c "getCdr"))
(define (nth lst n) (call lst "nth" n))
(define (nthCdr lst n) (call lst "nth" n))
(define (length lst) (call lst "length"))
