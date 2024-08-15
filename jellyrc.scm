;; entrypoint for the stdlib

;; for emacs inferior-scheme-mode
(define load loadFile)

;; load
(loadFile "base-base.scm")

;; srfi1 and its consequences
(loadFile "list-base.scm")
(loadFile "utils-for-list-library.scm")
(loadFile "list-library.scm")

(loadFile "printing-base.scm")

(loadFile "result-handling.scm")

;; import
(import (base base))
(import (printing base))

(import (list base))
(import (list library))

(import (result handling))
