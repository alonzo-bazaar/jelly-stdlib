;; entrypoint for the stdlib during normal usage
(define load loadFile) ;; for emacs inferior-scheme-mode

#|
sys keeps old mechanism
since app needs runtime to know about the sys library to pupulate it
|#
(import (jelly printing))
(import (jelly list base))
(loadFile "sys.scm")
