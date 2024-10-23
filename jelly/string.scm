(define-library (jelly string)
  (export len ref has iter sub)
  (import (jelly collection utils))
  (begin
    (define (len s)
      (call s "length"))
    (define (ref s i)
      (call s "charAt" i))
    (define (has s c)
      (ordmem s c
              ref len equal?))
    (define (iter fn str)
      ((iterating len ref) fn str))

    (define (sub s first &rest maybeLast)
      (let ((last
             (if (null? maybeLast)
                 (len s)
                 (car maybeLast))))
        (call s "substring" first last)))
    ))
