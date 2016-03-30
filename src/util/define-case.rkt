#lang racket

;; This module provides define-case which defines a set of tags and a case construct that is specific to those tags

(provide define-case)

(define-syntax define-case
  (syntax-rules ()
    [(_ case-name tag ...)
     (begin
       (define tag 'tag) ...
       (define-syntax case-name
         (letrec ([f (lambda (stx)
                       (syntax-case stx (tag ...)
                         [() #'()]
                         [([tag body (... ...)] . rest) #`([(tag) body (... ...)] . #,(f #'rest))] ...))])
           (lambda (stx)
             (syntax-case stx (else)
               [(_ l clauses (... ...) [else body (... ...)]) #`(case l #,@(f #'(clauses (... ...))) [else body (... ...)])]
               [(_ l clauses (... ...))                       #`(case l #,@(f #'(clauses (... ...))))])))))]))
