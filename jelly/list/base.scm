(define-library (jelly list base)
  (export map  any  every
          map1 any1 every1
          length
          forEach
          nth nthCdr
          append reverse

          set-car! set-cdr!
          pair? not-pair?
          first second third fourth fifth sixth seventh eight ninth tenth

          caar cadr cdar cddr
          caaar caadr cadar caddr cdaar cdadr cddar cdddr
          caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

          memq memv member
          optional)
  (begin
    (define (reverse lst)
      (do ((lst lst (cdr lst))
           (res nil (cons (car lst) res)))
          ((null? lst) res)))

    (define (complement pred)
      (lambda (x) (not (pred x))))

    (define (append lst1 lst2)
      (if (null? lst1)
          lst2
          (cons (car lst1) (append (cdr lst1) lst2))))

    (define (nthCdr lst n)
      (do ((lst lst (cdr lst))
           (n n (- n 1)))
          ((= n 0) lst)))

    (define (nth lst n)
      (car (nthCdr lst n)))

    (define (forEach fn lst)
      (do ((l lst (cdr l)))
          ((null? l) nil)
        (fn (car l))))

    (define (length lst)
      (do ((l lst (cdr l))
           (len 0 (+ 1 len)))
          ((null? l) len)))

    (define (map1 fn lst)
      (do ((iter lst (cdr iter))
           (acc nil (cons (fn (car iter)) acc)))
          ((null? iter) (reverse acc))))

    (define (map fn &rest lsts)
      (let ((acc nil))
        (do ((lsts lsts (map1 cdr lsts)))
            ((every null? lsts) (reverse acc))
          (set! acc (cons (apply fn (map1 car lsts)) acc)))))

    (define (any1 pred lst)
      (do ((lst lst (cdr lst)))
          ((or (null? lst)
               (pred (car lst)))
           (not (null? lst)))))

    (define (anyzip pred lsts)
      (do ((lsts lsts (map1 cdr lsts)))
          ((or (any1 null? lsts)
               (apply pred (map1 car lsts)))
           (not (any1 null? lsts)))))

    (define (any pred lst &rest lsts)
      (if (null? lsts)
          (any1 pred lst)
          (anyzip pred (cons lst lsts))))

    (define (every1 pred lst)
      ;; de morgan's
      (not (any1 (complement pred) lst)))

    (define (everyzip pred lsts)
      (do ((lsts lsts (map1 cdr lsts)))
          ((or (any1 null? lsts)
               (not (apply pred (map1 car lsts))))
           (any1 null? lsts))))

    (define (every pred lst &rest lsts)
      (if (null? lsts)
          (every1 pred lst)
          (everyzip pred (cons lst lsts))))

    (define (optional args default &rest n)
      (if (not (null? (cdr n))) (error "OPTIONAL : too many arguments"))
      (let ((index (if (null? n) 0 (car n))))
        (let ((candidate (nth args index)))
          (if (null? candidate)
              default
              candidate))))

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
