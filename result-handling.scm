(define-library (result handling)
  (export goodResult? resultGet)
  (begin
    (define (goodResult? result)
      (call result "isGood"))

    (define (resultGet result)
      (call result "get"))))
