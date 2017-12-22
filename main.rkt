#lang racket
(provide
 ;; data
 signal?
 pure-signal?
 value-signal?
 reactor?
 process?
 reactor-done?
 ;; running
 start
 react
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
 last
 last?
 suspend&
 abort&
 loop&
 halt&)
(require "data.rkt" "runtime.rkt" "compiler.rkt")

