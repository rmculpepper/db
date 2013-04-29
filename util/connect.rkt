#lang racket/base
(require db/base)
(provide connection-pool
         connection-pool?
         connection-pool-lease
         virtual-connection
         (rename-out [virtual-connection connection-generator])
         kill-safe-connection
         (struct-out data-source)
         dsn-connect
         current-dsn-file
         put-dsn
         get-dsn
         postgresql-data-source
         mysql-data-source
         sqlite3-data-source
         odbc-data-source)
