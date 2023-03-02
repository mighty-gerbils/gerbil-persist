#!/usr/bin/env gxi
;; To run tests, use: ./unit-tests.ss
;; You can even run tests without first building with ./build.ss !
(import :std/misc/process :clan/testing :clan/multicall)
(init-test-environment!)
(import :clan/persist/version)

(define-entry-point (build-and-test)
  (help: "Run all build and test commands" getopt: [])
  (run-process/batch ["./build.ss"])
  (run-process/batch ["./unit-tests.ss"])
  (run-process/batch ["./unit-tests.ss" "integration"]))
