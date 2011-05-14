#lang racket/base
(require racket/contract
         racket/list)

#|
Geometry according to mysql:
abstract Geometry
- Point = (x, y)
- abstract Curve
  - LineString = (list of points)
    - predicate Line
    - predicate LinearRing (closed and non-self-intersecting, bleh)
- abstract Surface
  - Polygon (defined by LinearRings, bleh)
      = exterior ring, list of interior rings
- GeometryCollection = (list of geometry values)
  - MultiPoint = (list of points)
  - abstract MultiCurve
    - MultiLineString = (list of line strings)
  - abstract MultiSurface
    - MultiPolygon = (list of polygons)

every geometric value has an associated Spacial Reference System (SRID), ignored by mysql

Geometry according to postgis:

same as above, but with coordinate variants: eg pointm = (x, y, m)
|#

(struct point (x y)
        #:transparent
        #:guard (lambda (x y _n)
                  (values (exact->inexact x)
                          (exact->inexact y))))

(struct line-string (points)
        #:transparent)

(define (line? x)
  (and (line-string? x)
       (let ([points (line-string-points x)])
         (and (= 2 (length points))
              (not (equal? (first points) (second points)))))))

(define (linear-ring? x)
  (and (line-string? x)
       (let ([points (line-string-points x)])
         ;; FIXME: require at least ??? points
         (equal? (first points) (last points)))))

(struct polygon (exterior interiors)
        #:transparent)

(struct multi-point (elements)
        #:transparent)

(struct multi-line-string (elements)
        #:transparent)

(struct multi-polygon (elements)
        #:transparent)

(struct geometry-collection (elements)
        #:transparent)

(define (geometry? x)
  (or (point? x)
      (line-string? x)
      (polygon? x)
      (multi-point? x)
      (multi-line-string? x)
      (multi-polygon? x)
      (geometry-collection? x)))

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
 [struct geometry-collection ([elements (listof geometry?)])])

;; ----------------------------------------

;; Based on OGC 06-103r4

(define (wkb->geometry b [start 0] [end (bytes-length b)])
  (bytes->geometry b start end #:srid? #f))

(define (bytes->geometry b [start 0] [end (bytes-length b)]
                         #:srid? [srid? #f])
  (define (get-byte)
    (begin0 (bytes-ref b start)
      (set! start (+ start 1))))
  (define (get-uint be?)
    (begin0 (integer-bytes->integer b #f be? start (+ start 4))
      (set! start (+ start 4))))
  (define (get-multi n get-X)
    (for/list ([i (in-range n)]) (get-X)))
  (define (get-geometry)
    (let ([srid (and srid? (get-uint #f))]
          [be? (zero? (get-byte))])
      (define (get-double)
        (begin0 (floating-point-bytes->real b be? start (+ start 8))
          (set! start (+ start 8))))
      (define (get-point)
        (let* ([x (get-double)]
               [y (get-double)])
          (point x y)))
      (define (get-linear-ring)
        (let ([len (get-uint be?)])
          (line-string (get-multi len get-point))))
      (let ([type (get-uint be?)])
        (case type
          ((1) (get-point))
          ((2) (let ([points (get-multi (get-uint be?) get-point)])
                 (line-string points)))
          ((3) (let ([rings (get-multi (get-uint be?) get-linear-ring)])
                 (when (null? rings)
                   (error 'wkb->geometry "polygon with zero rings"))
                 (polygon (car rings) (cdr rings))))
          ((4 5 6 7) (let ([constructor
                            (case type
                              ((4) multi-point)
                              ((5) multi-line-string)
                              ((6) multi-polygon)
                              ((7) geometry-collection))]
                           [elements (get-multi (get-uint be?) get-geometry)])
                       (constructor elements)))
          (else
           (error 'wkb->geometry "unsupported geometry type: ~s" type))))))
  (begin0 (get-geometry)
    (unless (= start end)
      (error 'wkb->geometry "~s bytes left over" (- end start)))))

(define (decode-big-endian? who b)
  (case b
    ((0) #t)
    ((1) #f)
    (else (error who "invalid byte-order marker: ~e" b))))

;; FIXME: define WKT, WKB functions here?
;; FIXME: eventually, integrate with geos?

(provide/contract
 [wkb->geometry
  (->* (bytes?)
       (exact-nonnegative-integer?
        exact-nonnegative-integer?)
       geometry?)]
 [bytes->geometry
  (->* (bytes?)
       (exact-nonnegative-integer?
        exact-nonnegative-integer?
        #:srid? any/c)
       geometry?)])
