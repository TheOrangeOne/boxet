#lang racket

(require net/http-client)
(require net/head)
(require net/url)
(require json)

(require "api-key.rkt")
(provide boxet)


;; short example of how to call dropbox-api... to be expanded upon soon.
(define (boxet)
  (define api-base "https://api.dropboxapi.com/2/")
  (define header-auth (string-append "Authorization: Bearer " api-key))
  (define header-type "Content-Type: application/json")
  (define data "{\"path\":\"\"}")

  (define url (string->url "https://api.dropboxapi.com/2/files/list_folder"))

  (define-values (status info resp)
    (http-sendrecv/url url
                       #:method "POST"
                       #:headers (list header-auth header-type)
                       #:data data))
  (define json (read-json resp))
  (define result (jsexpr->string json)))
