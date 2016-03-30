#lang racket

;; The "for/*" implementations in racket do not perform very well so this module implements more efficient versions of some of them

(provide for for/list for*/list for+/list)

(define-syntax for
  (syntax-rules ()
    [(_ ([lhs rhs]) body ...)
     (let loop ([list rhs])
       (if (null? list)
           (void)
           (begin (let ([lhs (car list)]) body ...) (loop (cdr list)))))]))

(define-syntax for/list
  (syntax-rules ()
    [(_ ([lhs rhs]) body ...)
     (let loop ([list rhs])
       (if (null? list)
           '()
           (cons (let ([lhs (car list)]) body ...) (loop (cdr list)))))]))

(define-syntax for*/list
  (syntax-rules ()
    [(_ ([lhs1 rhs1][lhs2 rhs2]) body ...)
     (let ()
       (define (loop1 list1)
         (if (null? list1)
             '()
             (loop2 (car list1) (cdr list1) (let ([lhs1 (car list1)]) rhs2))))
       (define (loop2 head1 tail1 list2)
         (if (null? list2)
             (loop1 tail1)
             (cons (let ([lhs1 head1][lhs2 (car list2)]) body ...) (loop2 head1 tail1 (cdr list2)))))
       (loop1 rhs1))]))

(define-syntax for+/list
  (syntax-rules ()
    [(_ ([lhs1 rhs1][lhs2 rhs2]) body ...)
     (let ([x rhs1])
       (if (null? x)
           '()
           (let ([y rhs2])
       (for*/list ([lhs1 x][lhs2 y]) body ...))))]))
