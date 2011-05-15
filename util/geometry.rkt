#lang racket/base
(require racket/contract
         "private/geometry.rkt")

(provide/contract
 [struct point ([x real?] [y real?])]
 [struct line-string ([points (listof point?)])]
 [line? (-> any/c boolean?)]
 [linear-ring? (-> any/c boolean?)]
 [struct polygon ([exterior linear-ring?]
                  [interiors (listof linear-ring?)])]
 [struct multi-point ([elements (listof point?)])]
 [struct multi-line-string ([elements (listof line-string?)])]
 [struct multi-polygon ([elements (listof polygon?)])]
 [struct geometry-collection ([elements (listof geometry?)])]

 [wkb->geometry
  (->* (bytes?)
       (exact-nonnegative-integer?
        exact-nonnegative-integer?)
       geometry?)])
