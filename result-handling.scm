;; result handling
(define (goodResult? result)
  (call result "isGood"))

(define (resultGet result)
  (call result "get"))
