(define-library (jelly printing)
  (export print    println    printty    printtyln
          errprint errprintln
          printList printtyList printtylnList
          newline)
  (begin
    ;; print
    (define (printSepListFn displayFn sep lst)
      (do ((l lst (cdr l)))
          ((null? l) nil)
        (displayFn (car l))
        (unless (null? (cdr l))
          (display sep))))

    (define (printSepList sep lst)
      (printSepListFn display sep lst))

    (define (errprintSepList sep lst)
      (printSepListFn errdisplay sep lst))

    (define (printSep sep &rest args)
      (printSepList sep args))

    (define (errprintSep sep &rest args)
      (errprintSepList sep args))

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

    (define (errprint &rest args)
      (errprintSepList "" args))

    (define (errprintln &rest args)
      (errprintSepList "" args)
      (newline))

    (define (println &rest args)
      (printSepList "" args)
      (newline))

    (define (displayty arg separator)
      (display arg)
      (display separator)
      (display (call (classOf arg) "getCanonicalName")))

    (define (errdisplayty arg separator)
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

    (define (printtylnList lst)
      (printtyList lst)
      (newline))))
