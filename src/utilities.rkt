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

(define make-html-table
  (λ (rows)
    (string-append "<table>" rows "</table>")))

(define make-p
  (λ (v)
    (string-append "<p>" v "</p>")))

(define make-a
  (λ (link text)
    (string-append "<a href=\"" link "\">" text "</a>")))

(define get-html-header
  (λ (back-link home-link style-source)
    (string-append
     "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\""
     style-source
     "\"></head><body><h5><span class=\"left\"><a href=\""
     back-link
     "\">Back</a></span><span class=\"right\"><a href=\""
     home-link
     "\">Jacob Adley</a></span></h5>")))

(define get-html-title
  (λ (title)
    (string-append
     "<h1>"
     title
     "</h1>")))

(define get-html-footer
  (λ ()
    "</body></html>"))

(define build-html-page
  (λ (back-link home-link style-source title)
    (λ (body)
      (string-append
       (get-html-header back-link home-link style-source)
       (get-html-title title)
       body
       (get-html-footer)))))


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
