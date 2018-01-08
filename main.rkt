#lang racket
(provide
 (contract-out
  ;; data
  [signal? predicate/c ]
  [pure-signal? predicate/c]
  [value-signal? predicate/c]
  [rename external-reactor? reactor? predicate/c]
  [process? predicate/c]
  [reactor-done? (-> (and/c external-reactor? reactor-safe?) any/c)]
  [reactor-suspended? (-> (and/c external-reactor? reactor-safe?) any/c)]
  [last (-> value-signal? any/c)]
  [last? (-> signal? any)]
  [reactor-safe? (-> external-reactor? any/c)]
  ;; running
  [prime (-> process? external-reactor?)]
  [react! (-> (and/c external-reactor? reactor-safe?) (or/c pure-signal? (list/c value-signal? any/c)) ... any)])
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
 halt&
 with-handlers&)
(require "data.rkt" "runtime.rkt" "compiler.rkt")

