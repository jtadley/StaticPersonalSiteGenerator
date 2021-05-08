#lang racket

(require "constants.rkt")

(provide (all-defined-out))


(define string-car
  (λ (str) (string-ref str 0)))

(define los→str
  (λ (los)
    (string-join los ", ")))

(define id (λ (x) x))

(define build-file-name
  (λ (name more)
    (if more
        (string-append name "-" more ".html")
        (string-append name ".html"))))

;; HTML -----------

(define make-td
  (λ (v)
    (string-append td-start v td-end)))

(define make-tr
  (λ (lo-td)
    (string-append tr-start (string-join lo-td) tr-end)))


;; STRING OPS ----------------------

(define line→field+value
  (λ (line)
    (let ([v (string-split line ":")])
      (values (car v) (cadr v)))))


;; FILE OPS ----------------------

(define dir→files
  (λ (dir)
    (map (λ (str) (string-append dir "/" str))
         (map path->string
              (directory-list dir)))))
