#lang scheme/base

(require
 (planet schematics/schemeunit:3)
 "grid-test.ss"
 "place-test.ss"
 "scan-match-test.ss")

(define/provide-test-suite all-tests
  grid-tests
  place-tests
  scan-match-tests)
