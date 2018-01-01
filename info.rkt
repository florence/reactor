#lang info

(define name "reactor")
(define collection "reactor")

(define version "0.1")

(define deps '(("base" #:version "6.10") "rackunit-lib"))

(define build-deps
  '("racket-doc" "scribble-lib" "rackunit"))

(define pkg-authors '("spencer@florence.io"))

(define scribblings '(("scribblings/reactor.scrbl")))
