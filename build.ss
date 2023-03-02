#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Gerbil-persist. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils
;;   gxpkg install github.com/fare/gerbil-crypto
;;   gxpkg install github.com/fare/gerbil-poo

(import :std/misc/process :clan/building :clan/multicall)
(init-build-environment!
 name: "Gerbil-persist"
 deps: '("clan" "clan/poo" "clan/crypto"))

(define-entry-point (build-and-test)
  (help: "Run all build and test commands" getopt: [])
  (compile)
  (run-process/batch ["./unit-tests.ss"])
  (run-process/batch ["./unit-tests.ss" "integration"]))
