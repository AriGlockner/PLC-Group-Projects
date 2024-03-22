; Ethan Hansen, Gabriel Wolf, Ari Glockner
; CSDS 345 Programming Language Concepts
; Interpreter Project 2
; Mar 2024

#lang racket

(provide (all-defined-out))

(require "simpleParser.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require "utils.rkt")


; putting it all together, take a filename, parse the file, and interpret the results
(define interpret
  (lambda (filename)
    (call/cc (lambda (return) 
    (cond
      ((string=? filename "") (error "need a non-empty filename"))
      (else
       
       (let* ((expressions (parser filename))
              (final-state (foldl (lambda (exp state) (M_state exp state return (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))) '() expressions)))
         final-state)
       
       ))))))