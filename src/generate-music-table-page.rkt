#lang racket

(require
 racket/struct)

(define DIR "../SiteSource/Music")
(define TITLE "Music")
(define BACK-PAGE "Music.html")
(define HOME-PAGE "../index.html")
(define OUT-FILE "out")
(define SEPARATOR ":")

;; -----

(struct table-data
  (artist
   album
   year
   genres
   rating
   opinion)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (data) 'table-data)
      (λ (data) (list (table-data-artist data)
                      (table-data-album data)
                      (table-data-year data)
                      (table-data-genres data)
                      (table-data-rating data)
                      (table-data-opinion data)))))])

(define get-empty-table-data
  (λ ()
    (table-data "" "" "" "" "" "")))

(define set-table-data
  (λ (data field value)
    (match field
      ["Artist" (table-data
                 value
                 (table-data-album data)
                 (table-data-year data)
                 (table-data-genres data)
                 (table-data-rating data)
                 (table-data-opinion data))]
      ["Album" (table-data
                (table-data-artist data)
                value
                (table-data-year data)
                (table-data-genres data)
                (table-data-rating data)
                (table-data-opinion data))]
      ["Year" (table-data
               (table-data-artist data)
               (table-data-album data)
               value
               (table-data-genres data)
               (table-data-rating data)
               (table-data-opinion data))]
      ["Genres" (table-data
                 (table-data-artist data)
                 (table-data-album data)
                 (table-data-year data)
                 value
                 (table-data-rating data)
                 (table-data-opinion data))]
      ["Rating" (table-data
                 (table-data-artist data)
                 (table-data-album data)
                 (table-data-year data)
                 (table-data-genres data)
                 value
                 (table-data-opinion data))]
      ["Opinion" (table-data
                  (table-data-artist data)
                  (table-data-album data)
                  (table-data-year data)
                  (table-data-genres data)
                  (table-data-rating data)
                  value)])))

(define get-table-data-header
  (λ () "<tr><td>Artist</td><td>Album</td><td>Year</td><td>Genres</td><td>Rating</td><td>Opinion</td></tr>"))

(define get-all-source-files-from-dir
  (λ (dir)
    (map (λ (str) (string-append dir "/" str))
         (map path->string
              (directory-list dir)))))

(define map-source-files-to-lines
  (λ (source-files)
    (map file->lines source-files)))

(define empty-string?
  (λ (str)
    (not (non-empty-string? str))))

(define string-car
  (λ (str)
    (substring str 0 1)))

(define string-cdr
  (λ (str)
    (substring str 1)))

(define get-field-value-from-line
  (λ (line)
    (letrec ([helper (λ (str field value after-sep?)
                       (cond
                         [(empty-string? str) (values field value)]
                         [else
                          (let ([a (string-car str)]
                                [d (string-cdr str)])
                            (cond
                              [(string=? a SEPARATOR) (helper d field value #t)]
                              [after-sep? (helper d field (string-append value a) after-sep?)]
                              [else (helper d (string-append field a) value after-sep?)]))]))])
      (helper line "" "" #f))))

(define file-lines→table-data
  (λ (file-lines)
    (match file-lines
      ['() (get-empty-table-data)]
      [`(,line . ,rest)
       (let-values ([(field value) (get-field-value-from-line line)]
                    [(rec) (file-lines→table-data rest)])
         (set-table-data rec field value))])))

(define map-file-lines-to-table-data
  (λ (file-lines)
    (map file-lines→table-data file-lines)))

(define table-data→list
  (λ (data)
    (list
     (table-data-artist data)
     (table-data-album data)
     (table-data-year data)
     (table-data-genres data)
     (table-data-rating data)
     (table-data-opinion data))))

(define make-html-td
  (λ (str)
    (string-append "<td>" str "</td>")))

(define list->string
  (λ (ls)
    (cond
      [(empty? ls) ""]
      [else
       (string-append (car ls) (list->string (cdr ls)))])))

(define table-data→html-row
  (λ (data)
    (string-append
     "<tr>"
     (list->string (map make-html-td (table-data→list data)))
     "</tr>")))

(define list-of-table-data→html-rows
  (λ (ls)
    (cond
      [(empty? ls) ""]
      [else (string-append
             (table-data→html-row (car ls))
             (list-of-table-data→html-rows (cdr ls)))])))

(define list-of-table-data->html-table
  (λ (ls)
    (string-append
     "<table>"
     (get-table-data-header)
     (list-of-table-data→html-rows ls)
     "</table>")))

(define build-html-table
  (λ (dir)
    (let* ([files (get-all-source-files-from-dir dir)]
           [lines (map-source-files-to-lines files)]
           [data (map-file-lines-to-table-data lines)]
           [table (list-of-table-data->html-table data)])
      table)))

(define get-html-header
  (λ (back-link home-link)
    (string-append
     "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\"styles.css\"></head><body><h5><span class=\"left\"><a href=\""
     back-link
     "\">Back</a></span><span class=\"right\"><a href=\""
     home-link
     "\">Jacob Adley</a></span></h5>")))

(define get-html-footer
  (λ ()
    "</body></html>"))

(define build-html-title
  (λ (title)
    (string-append
     "<h1>"
     title
     "</h1>")))

(define build-html-table-page-music
  (λ (dir title back-link home-link)
    (string-append
     (get-html-header back-link home-link)
     (build-html-title title)
     (build-html-table dir)
     (get-html-footer))))

(define build
  (λ ()
    (let ([page-str (build-html-table-page-music DIR TITLE BACK-PAGE HOME-PAGE)])
      (display-to-file page-str OUT-FILE))))

(build)
