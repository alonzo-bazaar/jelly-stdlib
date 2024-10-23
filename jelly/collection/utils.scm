(define-library (jelly collection utils)
  (export iterating ordmem)
  (begin
    (define (iterating lenFn refFn) 
      (lambda (fn seq)
        (let ((len (lenFn seq)))
          (do ((i 0 (+ i 1)))
              ((= i len))
            (fn (refFn seq i))))))
               
    (define (ordmem obj elt refFn lenFn test)
      (any (lambda (i) (test elt (refFn obj i))) (iota (lenFn obj))))))
