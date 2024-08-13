(define-library (base utils)
  (export memq memv member
          set-car! set-cdr!
          pair? not-pair?
          first second third fourth fifth sixth seventh eight ninth tenth
          caar cadr cdar cddr
          caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (begin
    (define (mem pred elt lst)
      (find-tail (curry pred elt) lst))

    (define (memq elt lst)
      (mem eq? elt lst))

    (define (memv elt lst)
      (mem eqv? elt lst))

    (define (member elt lst &rest maybe-predicate)
      (let ((pred (optional maybe-predicate equal?)))
        (mem pred elt lst)))

    (define set-car! setCar!)
    (define set-cdr! setCdr!)

    (define pair? cons?)
    (define (not-pair? x) (not (pair? x)))

    (define (first x)   (nth x 0))
    (define (second x)  (nth x 1))
    (define (third x)   (nth x 2))
    (define (fourth x)   (nth x 3))
    (define (fifth x)   (nth x 4))
    (define (sixth x)   (nth x 5))
    (define (seventh x) (nth x 6))
    (define (eight x)   (nth x 7))
    (define (ninth x)   (nth x 8))
    (define (tenth x)   (nth x 9))


    (define (caar   x) (car (car x)))
    (define (cadr   x) (car (cdr x)))
    (define (cdar   x) (cdr (car x)))
    (define (cddr   x) (cdr (cdr x)))

    (define (caaar  x) (caar (car x)))
    (define (caadr  x) (caar (cdr x)))
    (define (cadar  x) (cadr (car x)))
    (define (caddr  x) (cadr (cdr x)))
    (define (cdaar  x) (cdar (car x)))
    (define (cdadr  x) (cdar (cdr x)))
    (define (cddar  x) (cddr (car x)))
    (define (cdddr  x) (cddr (cdr x)))

    (define (caaaar x) (caaar (car x)))
    (define (caaadr x) (caaar (cdr x)))
    (define (caadar x) (caadr (car x)))
    (define (caaddr x) (caadr (cdr x)))
    (define (cadaar x) (cadar (car x)))
    (define (cadadr x) (cadar (cdr x)))
    (define (caddar x) (caddr (car x)))
    (define (cadddr x) (caddr (cdr x)))
    (define (cdaaar x) (cdaar (car x)))
    (define (cdaadr x) (cdaar (cdr x)))
    (define (cdadar x) (cdadr (car x)))
    (define (cdaddr x) (cdadr (cdr x)))
    (define (cddaar x) (cddar (car x)))
    (define (cddadr x) (cddar (cdr x)))
    (define (cdddar x) (cdddr (car x)))
    (define (cddddr x) (cdddr (cdr x)))))
