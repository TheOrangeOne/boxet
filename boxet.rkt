#lang racket

(require net/http-client)
(require net/head)
(require net/url)
(require json)

(require "api-key.rkt"
         "json-builder.rkt")
(provide list-directory
         list-directory-los)


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

;; (list-directory-jsexpr path recursive include_media_info include_deleted) -> jsexpr
;; path: string
;;   the path to the folder you want to see the contents of
;; recursive: boolean
;;   if #t, the list folder operation will be applied recursively to all subfolders and the response
;;   will contain contents of all subfolders, the default for this field is #f
;; include_media_info: boolean
;;   if #t, FileMetadata.media_info is set for photo and video. The default for this field is #f.
;; include_deleted: boolean
;;   if #t, the results will include entries for files and folders that used to exist but were deleted.
;;   the default for this field is #f.
(define (list-directory-jsexpr path [recursive #f] [media_info #f] [incl_deleted #f])
  (define end-point "files/list_folder")
  (define method "POST")
  (define data (build-json
                (make-json-obj
                 (list (make-json-kv "path" path)
                       (make-json-kv "recursive" (if recursive json-true json-false))
                       (make-json-kv "include_media_info" (if media_info json-true json-false))
                       (make-json-kv "include_deleted" (if incl_deleted json-true json-false))))))
  (call-api end-point method data))

(define (list-directory-json path)
  (jsexpr->string (list-directory-jsexpr path)))

;; returns a listified version of the json of the directory listing
(define (list-directory path [rec #f] [mi #f] [id #f])
  (define jsexpr (list-directory-jsexpr path rec mi id))
  (hash->strlist (safe-hash-ref jsexpr 'entries)))

;; returns a list of strings of the files and directories for the given path
(define (list-directory-los path [rec #f] [mi #f] [id #f])
  (filter (λ (x) (not (string=? x "name")))
          (flatten (map (λ (x) (filter (λ (x) (equal? (first x) "name")) x))
                        (list-directory path rec mi id)))))
