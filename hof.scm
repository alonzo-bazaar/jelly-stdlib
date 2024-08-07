(define (dolistFn fn lst)
  (do ((l lst (cdr l)))
      ((null? l) nil)
    (fn (car l))))

(define (map fn lst)
  (do ((iter lst (cdr iter))
        (acc nil (cons (fn (car iter)) acc)))
    ((null? iter) (reverse acc))))

(define (reduce-r fn lst initial)
  (do ((val initial (fn (car lst) val))
       (lst lst (cdr lst)))
      ((null? lst) val)))

(define (reduce-l fn lst initial)
  (do ((val initial (fn val (car lst)))
       (lst lst (cdr lst)))
      ((null? lst) val)))

(define reduce reduce-l)

(define (all? check lst)
  (reduce (lambda (a b) (and a b))
          (map check lst)
          #t))

(define (any? check lst)
  (reduce (lambda (a b) (or a b))
          (map check lst)
          #f))

(define (allEqual? key lst)
  (let ((firstVal (key (car lst)))
        (otherVals (map key (cdr lst))))
    (all? (lambda (x) (equal? x firstVal)) otherVals)))
