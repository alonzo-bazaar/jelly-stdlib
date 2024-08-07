(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))

(define (procedure? fn)
  (call (findClass "org.jelly.eval.evaluable.procedure.Procedure")
        "isAssignableFrom"
        (classOf fn)))

(define (last-pair lis)
  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))

(define (lor a b) (or a b))
(define (land a b) (and a b))
(define (all lst) (reduce land lst #t))
(define (any lst) (reduce lor lst #f))

(define (zero? x) (= x 0))
(define (even? x) (= (mod x 2) 0))
(define (odd? x) (= (mod x 2) 1))

(define (filter pred lst)
  (let ((acc nil))
    (dolistFn 
     (lambda (x) (if (pred x) (set! acc (cons x acc))))
     lst)
    (reverse acc)))

(define (count pred &rest lsts)
  (length (filter id (apply map (cons pred lsts)))))
        
(define (unfold-right f p g &rest maybe-tail)
  (let lp ((seed seed) (lis tail))
    (if (p seed) lis
        (lp (g seed)
            (cons (f seed) lis)))))

(define (optional args default &rest n)
  (if (not (null? (cdr n))) (error "OPTIONAL : too many arguments"))
  (let ((index (if (null? n) 0 (car n))))
    (let ((candidate (nth index args)))
      (if (null? candidate)
          default
          candidate))))
