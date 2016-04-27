#lang racket

(require net/http-client)
(require net/head)
(require net/url)
(require json)

(require "api-key.rkt"
         "json-builder.rkt")
(provide list-directory)

(define api-base "https://api.dropboxapi.com/2/")
(define header-type "Content-Type: application/json")
(define header-auth (string-append "Authorization: Bearer " api-key))

(define (safe-hash-ref h k)
  (if (and (hash? h) (hash-has-key? h k)) (hash-ref h k) (make-hash)))

(define (hash->strlist h)
  (cond [(list? h) (map hash->strlist h)]
        [(symbol? h) (symbol->string h)]
        [(not (hash? h)) h]
        [else (hash-map h (lambda (k v) (list (hash->strlist k) (hash->strlist v))))]))

(define (call-api endpoint method data)
  (define-values (status header resp)
    (http-sendrecv/url (string->url (string-append api-base endpoint))
                       #:method method
                       #:headers (list header-auth header-type)
                       #:data data))
  (read-json resp))

;; return listing of the directory given in json format
(define (list-directory-jsexpr dir)
  (define end-point "files/list_folder")
  (define method "POST")
  ;(define data (string-append "{\"path\":\"" dir "\"}"))
  (define data (build-json (make-json-obj (list (make-json-kv "path" dir)))))
  (call-api end-point method data))

(define (list-directory-json dir)
  (jsexpr->string (list-directory-jsexpr dir)))


(define (list-directory dir)
  (define jsexpr (list-directory-jsexpr dir))
  (hash->strlist (safe-hash-ref jsexpr 'entries)))


(define (boxet)
  (list-directory ""))


(boxet)
