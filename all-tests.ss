#lang scheme/base

(require
 (planet schematics/schemeunit:3)
 "grid-test.ss"
 "prior-test.ss")

(define/provide-test-suite all-tests
  grid-tests
  prior-tests)
