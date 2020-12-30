(export content-addressing-test)

(import
  :std/sugar :std/test :std/text/hex
  ../content-addressing)

(def content-addressing-test
  (test-suite "test suite for persist/content-addressing"
    (test-case "digest<-file"
      (check-equal? (hex-encode (digest<-file "/dev/null")) "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"))))
