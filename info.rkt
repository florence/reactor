#lang info

(define name "reactor")
(define collection "reactor")

(define version "0.1")

(define deps '("seq-no-order"
               ("base" #:version "6.12") "rackunit-lib"))

(define build-deps
  '("racket-doc" "scribble-lib" "rackunit"))

(define pkg-authors '("spencer@florence.io"))

(define scribblings '(("scribblings/reactor.scrbl")))
