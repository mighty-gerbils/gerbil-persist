#!/usr/bin/env gxi
;; -*- Gerbil -*-
;; This is the main build file for Gerbil-persist. Invoke it using
;; ./build.ss [cmd]
;; where [cmd] is typically left empty (same as "compile")
;; Note that may you need to first:
;;   gxpkg install github.com/fare/gerbil-utils
;;   gxpkg install github.com/fare/gerbil-crypto
;;   gxpkg install github.com/fare/gerbil-poo

(import
  :std/make :std/misc/list :std/srfi/1
  :utils/filesystem :utils/path :utils/versioning)

(def here (path-parent (this-source-file)))
(current-directory here)

(def (files)
  (filter (lambda (x) (and (path-extension-is? x ".ss")
                      (not (member x '("build.ss" "unit-tests.ss")))))
          (directory-files ".")))

(def gsc-options/no-optimize '("-cc-options" "-O0" "-cc-options" "-U___SINGLE_HOST"))
(def gsc-options/tcc '("-cc" "tcc" "-cc-options" "-shared"))

(def (tcc?) (case (getenv "USE_TCC" #f) (("y" "Y" "yes" "YES" "1") #t) (else #f)))
(def (optimize?) (case (getenv "USE_OPT" #f) (("n" "N" "no" "NO" "0") #f) (else #t)))
(def (debug?) (case (getenv "USE_DBG" #f) (("src") 'src) (("env") 'env) (else #f)))

(def gsc-options
  (append (when/list (tcc?) gsc-options/tcc)
          (when/list (not (optimize?)) gsc-options/no-optimize)))

(def (normalize-spec x)
  (match x
    ((? string?) [gxc: x . gsc-options])
    ([(? (cut member <> '(gxc: gsc: exe:))) . _] (append x gsc-options))))

(def (build-spec)
  (map normalize-spec (files)))

(def (main . args)
  (when (match args ([] #t) (["compile" . _] #t) (_ #f))
    (update-version-from-git name: "Gerbil-persist" path: "version.ss"))
  (make (build-spec)
    srcdir: here
    verbose: #f
    debug: 'env
    optimize: #t))
