(define-library (jelly array)
  (import (jelly collection utils)
          (only (jelly functional) curry))
  (export len set ref has iter)
  (begin
    (define arrClass (findClass "java.lang.reflect.Array"))
    (define (callArr methName &rest args)
      (apply callStatic (cons* arrClass methName args)))
    (define len (curry callArr "getLength"))
    (define ref (curry callArr "get"))
    (define set (curry callArr "set"))
    (define (has a o)
      (ordmem a o
              ref len equal?))

    (define (iter fn arr)
      ((iterating len ref) fn arr))
    ))
