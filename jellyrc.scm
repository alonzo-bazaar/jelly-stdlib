;; entrypoint for the stdlib during normal usage
(define load loadFile) ;; for emacs inferior-scheme-mode

(loadFile "printing-base.scm")
(import (printing base))
;; (println "we print debugging the standard library, boy")

(loadFile "base-base.scm")

;; srfi1 and its consequences
(loadFile "list-base.scm")
(loadFile "utils-for-list-library.scm")
(loadFile "list-library.scm")

(loadFile "result-handling.scm")
(loadFile "sys.scm")

;; import
(import (base base))
(import (list base))
(import (list library))
(import (result handling))
(import (sys))
