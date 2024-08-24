(define-library (utils for list library)
  (import (base base))
  (export procedure?
          complement
          zero? even? odd?
          optional
          or- and-)
  ;; add it to java for fuck's sake
  (define (procedure? fn)
    (call (findClass "org.jelly.eval.evaluable.procedure.Procedure")
          "isAssignableFrom"
          (classOf fn)))

  ;; general utils
  (define (complement pred)
    (lambda (x) (not (pred x))))

  (define (zero? x) (= x 0))
  (define (even? x) (= (mod x 2) 0))
  (define (odd? x) (= (mod x 2) 1))

  (define (optional args default &rest n)
    (if (not (null? (cdr n))) (error "OPTIONAL : too many arguments"))
    (let ((index (if (null? n) 0 (car n))))
      (let ((candidate (nth args index)))
        (if (null? candidate)
            default
            candidate))))

  (define (or- a b) (or a b))
  (define (and- a b) (and a b)))
