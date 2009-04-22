
#lang scheme/base
(require scheme/unit
         ;; "sql-format.ss"
         )
(provide database^)

(define-signature database^
  (connect
   dbsystem))
