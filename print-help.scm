;; print
(define (printSepList sep lst)
  (do ((l lst (cdr l)))
      ((null? l) nil)
    (display (car l))
    (unless (null? (cdr l))
      (display sep))))

(define (printSep sep &rest args)
  (printSepList sep args))

(define (printtySepList tySep eltSep lst)
  (do ((l lst (cdr l)))
      ((null? l) nil)
    (displayty (car l) tySep)
    (unless (null? (cdr l))
      (display eltSep))))

(define (printtySep tySep eltSep &rest args)
  (do ((a args (cdr a)))
      ((null? a) nil)
    (displayty (car a) tySep)
    (unless (null? (cdr a))
      (display eltSep))))

(define (print &rest args)
  (printSepList "" args))

(define (println &rest args)
  (printSepList "" args)
  (newline))

(define (displayty arg separator)
  (display arg)
  (display separator)
  (display (call (classOf arg) "getCanonicalName")))

(define (printty &rest args)
  (printtySepList #\: "" args))

(define (printtyln &rest args)
  (printtySepList #\: "" args)
  (newline))

(define (newline) (display #\Newline))

(define terpri newline)

(define (printList lst)
  (display "(")
  (printSepList " " lst)
  (display ")"))

(define (printtyList lst)
  (display "(")
  (printtySepList #\: #\Space lst)
  (display ")"))
