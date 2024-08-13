(define-library (misc utils)
  (export dolistFn
          cars cdrs
          curry rcurry
          id)
  (begin
    ;; base utils
    (define (dolistFn fn lst)
      (do ((l lst (cdr l)))
          ((null? l) nil)
        (fn (car l))))

    (define (cars lst) (map car lst))
    (define (cdrs lst) (map cdr lst))

    (define (lcurry fn first)
      (lambda (x) (fn x first)))

    (define (rcurry fn first)
      (lambda (x) (fn first x)))

    (define curry lcurry)

    (define (id x) x)))
