#lang racket

(require rackunit json)
(provide (all-defined-out))


(define (remove-spaces str)
  (string-replace str " " ""))

;; a json array is a (listof (anyof string json-array json-obj))
(define-struct json-array (items) #:prefab)

;; a json-obj is a (listof json-kv)
(define-struct json-obj (kvs) #:prefab)

;; a kv is a
;; key: string
;; value: (anyof string json-array json-obj)
(define-struct json-kv (k v) #:prefab)


(define json-true #t)

(define json-false #f)


;; takes a valid json object or array (json-obj, json-array)
;; and produces the corresponding string representation
;; eg. (build-json (make-json-obj (list (make-json-kv "key1" "val1")
;;                                      (make-json-kv "key2" "val2")
;;                                      (make-json-kv "key3" "val3"))))
;; produces:
;; {
;;   key1: val1,
;;   key2: val2,
;;   key3: val3
;; }
(define (build-json expr)
  (cond [(empty? expr) ""]
        [(number? expr) (number->string expr)]
        [(equal? json-true expr) "true"]
        [(equal? json-false expr) "false"]
        [(equal? json-null expr) "null"]
        [(string? expr) (string-append "\"" expr "\"")]
        [(json-array? expr)
         (string-append "[" (string-join (map build-json (json-array-items expr)) ",") "]")]
        [(json-kv? expr)
         (string-append (build-json (json-kv-k expr)) ":" (build-json (json-kv-v expr)))]
        [(json-obj? expr)
         (define kvs (json-obj-kvs expr))
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
                      (make-json-kv "u" (make-json-array (list "1" 2 json-null json-false json-true))))))
   (remove-spaces "{ \"x\": { \"a\": [\"x\", \"y\", \"z\"] }, \"u\": [\"1\", 2, null, false, true] }"))

  (check-equal?
   (hash? (string->jsexpr (build-json (make-json-obj (list (make-json-kv "a" (make-json-array (list "1" "b")))))))) #t))

(define (build-jsexpr expr)
  (string->jsexpr (build-json expr)))
