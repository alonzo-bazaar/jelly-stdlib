;; jellyrc for testing
;; only loads libraries used in the (java) unit tests

(loadFile "printing-base.scm")
(import (printing base))

(loadFile "base-base.scm")
(import (base base))
