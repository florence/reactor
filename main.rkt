#lang racket
(provide
 (contract-out
  ;; data
  [signal? predicate/c ]
  [pure-signal? predicate/c]
  [value-signal? predicate/c]
  [reactor? predicate/c]
  [tree? predicate/c]
  [struct branch ([values list?] [children (listof tree?)]) #:omit-constructor]
  [struct leaf ([values list?]) #:omit-constructor]
  [reactor-done? (-> (and/c reactor? reactor-safe?) any/c)]
  [reactor-suspended? (-> (and/c reactor? reactor-safe?) any/c)]
  [last (-> value-signal? any)]
  [last? (-> signal? any/c)]
  [default (-> value-signal? any)]
  [reactor-safe? (-> reactor? any/c)]
  [signal-name (-> signal? (and/c symbol? (not/c symbol-interned?)))]
  [signal=? (-> signal? signal? any/c)]
  [reactor-continuation-marks
   (-> (and/c reactor? reactor-safe?)
       continuation-mark-set-tree?)]
  [continuation-mark-set-tree->tree
   (-> continuation-mark-set-tree? any/c tree?)]
  ;; running
  [prime (->i ([p (args) (procedure-arity-includes/c (length args))])
              #:rest [args any/c]
              [_ reactor?])]
  [react! (-> (and/c reactor? reactor-safe?) (or/c pure-signal? (list/c value-signal? (listof any/c))) ... any)])
 ;; forms
 signal/c
 present&
 (rename-out [signal* signal])
 define-signal
 par&
 emit&
 pause&
 await&
 await*&
 suspend&
 abort&
 loop&
 halt&
 with-handlers&
 reactive->)
(require "data.rkt" "runtime.rkt" "compiler.rkt" "ct.rkt")

(module reader syntax/module-reader
  reactor/lang)


