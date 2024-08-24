;; entrypoint for the stdlib during normal usage
(define load loadFile) ;; for emacs inferior-scheme-mode

(loadFile "printing-base.scm")
(import (printing base))

(loadFile "base-base.scm")
; (import (base base))

(loadFile "list-base.scm")
; (import (list base))

(loadFile "utils-for-list-library.scm")
(loadFile "list-library.scm")
(import (list library))

(loadFile "result-handling.scm")
(import (result handling))

(loadFile "sys.scm")
(import (sys))
