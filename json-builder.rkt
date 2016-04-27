#lang racket

(require rackunit json)
(provide build-json)


(define (remove-spaces str)
  (string-replace str " " ""))

;; a json array is a (listof (anyof string json-array json-obj))
(define-struct json-array (items))

;; a json-obj is a (listof json-kv)
(define-struct json-obj (kvs))

;; a kv is a
;; key: string
;; value: (anyof string json-array json-obj)
(define-struct json-kv (k v))


;; takes a list of keys and values (list key1 val1 key2 val2 key3 val3 ... keyn valn)
;; (typeof key) => string
;; (typeof val) => (anyof string json-array json-obj)
;; and produces a string representing the json
;; {
;;   key1: val1,
;;   key2: val2,
;;   key3: val3
;; }
(define (build-json item)
  (cond [(empty? item) ""]
        [(string? item) (string-append "\"" item "\"")]
        [(number? item) item]
        [(json-array? item)
         (string-append "[" (string-join (map build-json (json-array-items item)) ",") "]")]
        [(json-kv? item)
         (string-append (build-json (json-kv-k item)) ":" (build-json (json-kv-v item)))]
        [(json-obj? item)
         (define kvs (json-obj-kvs item))
         (string-append "{" (string-join (map build-json kvs) ",") "}")]))

(test-case
  "build json"
  (check-equal?
   (build-json (make-json-obj (list (make-json-kv "x" "y"))))
   (remove-spaces "{ \"x\": \"y\" }"))

  (check-equal?
   (build-json (make-json-obj (list (make-json-kv "x" "y")
                                    (make-json-kv "u" "v"))))
   (remove-spaces "{ \"x\": \"y\", \"u\": \"v\" }"))

  (check-equal?
   (build-json (make-json-obj
                (list (make-json-kv "x" (make-json-array (list "a" "b" "c")))
                      (make-json-kv "u" "v"))))
   (remove-spaces "{ \"x\": [\"a\", \"b\", \"c\"], \"u\": \"v\" }"))

  (check-equal?
   (build-json (make-json-obj
                (list (make-json-kv "x" (make-json-obj (list (make-json-kv "a" "b"))))
                      (make-json-kv "u" "v"))))
   (remove-spaces "{ \"x\": { \"a\": \"b\" }, \"u\": \"v\" }"))

  (check-equal?
   (build-json (make-json-obj
                (list (make-json-kv "x" (make-json-obj (list (make-json-kv "a" (make-json-array (list "x" "y" "z"))))))
                      (make-json-kv "u" "v"))))
   (remove-spaces "{ \"x\": { \"a\": [\"x\", \"y\", \"z\"] }, \"u\": \"v\" }"))

  (check-equal?
   (build-json (make-json-obj
                (list (make-json-kv "x" (make-json-obj (list (make-json-kv "a" (make-json-array (list "x" "y" "z"))))))
                      (make-json-kv "u" (make-json-array (list "1" "2" "3"))))))
   (remove-spaces "{ \"x\": { \"a\": [\"x\", \"y\", \"z\"] }, \"u\": [\"1\", \"2\", \"3\"] }"))

  (check-equal?
   (hash? (string->jsexpr (build-json (make-json-obj (list (make-json-kv "a" (make-json-array (list "1" "b"))))))))
   #t))
