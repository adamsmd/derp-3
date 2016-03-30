#lang racket

;; This module provides unsafe-struct* which is a more efficient (but lacking dynamic type checks and thus unsafe) version of struct

(require (for-syntax racket/syntax))
(require racket/performance-hint)
(require racket/unsafe/ops)

(provide unsafe-struct*)

(define-syntax (unsafe-struct* stx)
  (define (std-field f)
    (syntax-case f ()
      [(_ . _) f]
      [x #'(x)]))
  (syntax-case stx ()
    [(_ name (fields ...) options ...)
     (with-syntax* ([(std-fields ...) (map std-field (syntax->list #'(fields ...)))]
                    [(field-index ...) (datum->syntax #'name (build-list (length (syntax->datum #'(fields ...))) values))]
                    [((field-name . field-rest) ...) #'(std-fields ...)]
                    [(hidden-field ...) (generate-temporaries #'(field-name ...))]
                    [(getters ...) (map (lambda (x) (format-id x "~a-~a" #'name x)) (syntax->list #'(field-name ...)))]
                    [(setters ...) (map (lambda (x) (format-id x "~a-~a-set!" #'name x)) (syntax->list #'(field-name ...)))])
      #`(begin
          (struct name ([hidden-field . field-rest] ...) options ...)
          (define-inline (getters n) (unsafe-struct*-ref n field-index)) ...
          (define-inline (setters n v) (unsafe-struct*-set! n field-index v)) ...
          ))]))
