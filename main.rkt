#lang racket
(provide
 (contract-out
  ;; data
  [signal? predicate/c ]
  [pure-signal? predicate/c]
  [value-signal? predicate/c]
  [reactor? predicate/c]
  [process? predicate/c]
  [reactor-done? (-> reactor? any)]
  [reactor-suspended? (-> reactor? any)]
  [last (-> value-signal? any/c)]
  [last? (-> signal? any)]
  ;; running
  [prime (-> process? reactor?)]
  [react! (-> reactor? (or/c pure-signal? (list/c value-signal? any/c)) ... any)])
 ;; process creation
 (rename-out [process* process])
 define-process
 ;; forms
 run&
 present&
 (rename-out [signal* signal])
 define-signal
 par&
 emit&
 pause&
 await&
 suspend&
 abort&
 loop&
 halt&)
(require "data.rkt" "runtime.rkt" "compiler.rkt")

