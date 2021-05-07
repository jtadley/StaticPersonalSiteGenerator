#lang racket

(require
 racket/struct)

(define DIR "../SiteSource/Music")
(define TITLE "All Music")
(define BACK-PAGE "index.html")
(define HOME-PAGE "../../index.html")
(define STYLE-SOURCE "../../styles.css")
(define SEPARATOR ":")
(define OUT-PREFIX "../out/")

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
  (λ (dir filter-pred sort-fn)
    (let* ([files (get-all-source-files-from-dir dir)]
           [lines (map-source-files-to-lines files)]
           [data (map-file-lines-to-table-data lines)]
           [data (if filter-pred (filter filter-pred data) data)]
           [data (if sort-fn (sort data sort-fn) data)]
           [table (list-of-table-data->html-table data)])
      table)))

(define get-html-header
  (λ (back-link home-link)
    (string-append
     "<!DOCTYPE html><html><head><link rel=\"stylesheet\" href=\""
     STYLE-SOURCE
     "\"></head><body><h5><span class=\"left\"><a href=\""
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
  (λ (dir title back-link home-link filter-pred sort-fn)
    (string-append
     (get-html-header back-link home-link)
     (build-html-title title)
     (build-html-table dir filter-pred sort-fn)
     (get-html-footer))))

(define build
  (λ (out-file filter-pred sort-fn)
    (let ([page-str (build-html-table-page-music DIR TITLE BACK-PAGE HOME-PAGE filter-pred sort-fn)])
      (display-to-file page-str out-file))))

(define build-all
  (λ ()
    (build "All.html" #f (λ (td1 td2)
                           (< (string->number (table-data-rating td1)) (string->number (table-data-rating td2)))))))

(define RATINGS (list 1 2 3 4 5 6 7 8 9 10))

(define build-numbers
  (λ (lon δ title accessor)
    (let ([title-suffix (if (zero? δ)
                            ""
                            "s")])
      (letrec ([helper (λ (ls)
                        (if (not (empty? ls))
                            (begin
                              (build
                               (string-append title "-" (number->string (car ls)) title-suffix ".html")
                               (λ (td)
                                 (let ([cur-num (string->number (accessor td))])
                                   (if (zero? δ)
                                       (equal? cur-num (car ls))
                                       (and
                                        (>= cur-num (car ls))
                                        (< cur-num (+ (car ls) δ))))))
                               (if (zero? δ)
                                   #f
                                   <))
                              (helper (cdr ls)))
                            (writeln (string-append "done-" title))))])
        (helper lon)))))

(define build-rating
  (λ ()
    (build-numbers RATINGS 0 "Rating" (λ (td) (table-data-rating td)))))

(define YEARS (list 1960 1970 1980 1990 2000 2010 2020))

(define build-year
  (λ ()
    (build-numbers YEARS 10 "Year" (λ (td) (table-data-year td)))))

(define ALPHABET (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define build-alphabet
  (λ (title accessor)
    (let* ([leading-char (λ (td) (string-ref (accessor td) 0))]
           [sort-fn string<=?])
      (letrec ([helper (λ (ls)
                         (if (not (empty? ls))
                             (begin
                               (build
                                (string-append title "-" (string (car ls)) ".html")
                                (λ (td)
                                  (equal? (car ls) (char-upcase (leading-char td))))
                                sort-fn)
                               (helper (cdr ls)))
                             (begin
                               (build
                                (string-append title "-Other.html")
                                (λ (td)
                                  (not (char-alphabetic? (leading-char td))))
                                sort-fn)
                               (writeln (string-append "done-" title)))))])
        (helper ALPHABET)))))

(define build-artist
  (λ ()
    (build-alphabet "Artist" (λ (td) (table-data-artist td)))))

(define build-album
  (λ ()
    (build-alphabet "Album" (λ (td) (table-data-album td)))))

(define build-in-list
  (λ (ls title accessor)
    (if (empty? ls)
        (writeln (string-append "done-" title))
        (begin
          (build
           (string-append title "-" (car ls) ".html")
           (λ (td) (string-contains? (accessor td) (car ls)))
           (λ (td1 td2) (< (table-data-rating td1) (table-data-rating td2))))
          (build-in-list (cdr ls) title accessor)))))

(define GENRES (list "Shoegaze" "Noise" "Industrial" "Jazz" "Art"))

(define build-genre
  (λ ()
    (build-in-list GENRES "Genre" (λ (td) (table-data-genres td)))))

(define go
  (λ ()
    (build-all)
    (build-rating)
    (build-year)
    (build-artist)
    (build-album)
    (build-genre)
    ))
