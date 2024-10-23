(define-library (jelly functional)
  ;; some haskellerie
  (export curry flip complement id)
  (begin
    (define (id x) x)
    (define (complement pred)
      (lambda (x) (not (pred x))))
    (define (curry fn x)
      (lambda (&rest args) (apply fn (cons x args))))
    (define (flip fn)
      (lambda (x y) (fn y x)))))
