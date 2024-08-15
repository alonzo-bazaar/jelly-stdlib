(define-library (base base)
  ;; only library (yet) to be loaded by the runtime during unit test
  ;; because loading the whole thing would be very expensive
  (export map any every
          map1 any1 every1
          forEach
          id)
  (begin
    (define (forEach fn lst)
      (do ((l lst (cdr l)))
          ((null? l) nil)
        (fn (car l))))

    (define (map1 fn lst)
      (do ((iter lst (cdr iter))
           (acc nil (cons (fn (car iter)) acc)))
          ((null? iter) (reverse acc))))

    (define (map fn &rest lsts)
      (let ((acc nil))
        (do ((lsts lsts (map1 cdr lsts)))
            ((all? null? lsts) (reverse acc))
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

    (define (id x) x)))
