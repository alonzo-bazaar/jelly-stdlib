(define-library (jelly ctype)
  ;; #include<ctype.h>
  (export space?)
  (begin
    (define (space? ch)
      (callStatic (findClass "java.lang.Character") "isWhitespace" ch))))
