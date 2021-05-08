#lang racket

(require "utilities.rkt"
         "constants.rkt"
         racket/struct)

(provide (all-defined-out))

;; --------------

(define SOURCE-DIR "../SiteSource/Music")
(define BACK-PAGE "index.html") ; this should be dynamic
(define HOME-PAGE "../index.html")
(define STYLE-SOURCE "../styles.css")
(define OUT-DIR "out")
(define SEPARATOR ":")

;; --------------

(struct table
  (file_name
   data_rows)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (t) 'table)
      (λ (t) (list (table-file_name t)
                   (table-data_rows t)))))])

(struct data
  (name        ; title (ex: "Artist")
   accessor    ; get the value for this data from a struct
   setter      ; takes a value and a struct and sets the name element
   to_string   ; get the value as a printable string
   table?      ; should we generate a table sorted by this
   ;; info for generating tables
   portions    ; #f OR list of things we are splitting the data into
   in_portion? ; #f OR predicate that takes in an element and says whether it belongs to a portion
   portion→str ; convert an element of portions to a string
   sort_fn     ; #f OR takes two elements of a struct and compares them
   overflow?   ; #f OR predicate that takes in an element and says whether it doesn't match a portion
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (d) 'data)
      (λ (d) (list (data-name d)
                   (data-table? d)))))])

(define struct→tables
  (λ (d lo-struct)
    (let ([d-name (data-name d)]
          [d-portions (data-portions d)]
          [d-in_portion? (data-in_portion? d)]
          [d-portion→str (data-portion→str d)]
          [d-sort_fn (data-sort_fn d)]
          [d-overflow? (data-overflow? d)])
      (letrec ([helper
                (λ (ls)
                  (cond
                    [(empty? ls)
                     (if d-overflow?
                         (list (table (build-file-name d-name "Other")
                                      (filter d-overflow? lo-struct)))
                         '())]
                    [else
                     (let* ([cur (if d-in_portion? (filter (d-in_portion? (car ls)) lo-struct) lo-struct)]
                            [cur (if d-sort_fn (sort cur d-sort_fn) cur)])
                       (cons
                        (table (build-file-name d-name (d-portion→str (car ls)))
                               cur)
                        (helper (cdr ls))))]))])
        (helper d-portions)))))

(define add-field+value-to-data
  (λ (d fld val lod)
    (cond
      [(empty? lod) (writeln fld) d]
      [(string=? fld (data-name (car lod)))
       ((data-setter (car lod))
        val
        d)]
      [else (add-field+value-to-data d fld val (cdr lod))])))

(define lines→data
  (λ (lod get-default-struct)
    (λ (lines)
      (cond
        [(null? lines) (get-default-struct)]
        [else (let-values ([(fld val) (line→field+value (car lines))]
                           [(rec) ((lines→data lod get-default-struct) (cdr lines))])
                (add-field+value-to-data
                 rec
                 fld
                 val
                 lod))]))))

(define lod→table-header
  (λ (lod)
    (let ([names (map (λ (d) (make-td (data-name d))) lod)])
      (make-tr names))))

(define struct→tr
  (λ (lod)
    (let ([lo-to_string (map data-to_string lod)])
      (λ (s)
        (letrec ([helper (λ (ls)
                           (cond
                             [(empty? ls) '()]
                             [else
                              (cons ((car ls) s)
                                    (helper (cdr ls)))]))])
          (make-tr (map make-td (helper lo-to_string))))))))

(define lo-struct→tables
  (λ (lod lo-struct)
    (cond
      [(empty? lod) '()]
      [(not (data-table? (car lod))) (lo-struct→tables (cdr lod) lo-struct)]
      [else
       (append
        (struct→tables (car lod) lo-struct)
        (lo-struct→tables (cdr lod) lo-struct))])))

(define table→html
  (λ (home-link style-source title)
    (λ (lod)
      (λ (t)
        (let-values ([(name portion) (get-info-from-file-name (table-file_name t))])
          (let* ([table-header (lod→table-header lod)]
                 [table-rows (map (λ (t) ((struct→tr lod) t)) (table-data_rows t))]
                 [body (make-html-table (string-append table-header (string-join table-rows)))]
                 [html ((build-html-page (string-append name ".html") home-link style-source (string-append title " by " name ": " portion)) body)])
            (table (table-file_name t) html)))))))

(define html→disk
  (λ (out-dir)
    (λ (t)
      (display-to-file (table-data_rows t)
                       (string-append out-dir "/" (table-file_name t))))))

(define get-root-file-name
  (λ (d)
    (string-append (data-name d) ".html")))

(define get-index-file
  (λ (back-link home-link style-source title)
    (λ (lod)
      (let* ([pas (map (λ (d)
                         (make-p (make-a (get-root-file-name d)
                                         (data-name d))))
                       lod)]
             [body (string-join pas)])
        (table
         "index.html"
         ((build-html-page back-link home-link style-source title) body))))))

(define get-root-file
  (λ (back-link home-link style-source title)
    (λ (d)
      (let* ([d-name (data-name d)]
             [d-portions (data-portions d)]
             [d-portion→str (data-portion→str d)]
             [d-portions-str (map d-portion→str d-portions)]
             [get-cur-file-name (λ (p) (build-file-name d-name p))]
             [pas (map (λ (p)
                         (make-p (make-a (get-cur-file-name p)
                                         p)))
                       d-portions-str)]
             [body (string-join pas)])
        (table
         (string-append d-name ".html")
         ((build-html-page back-link home-link style-source (string-append title " by " d-name)) body))))))

(define go
  (λ (lod get-default-struct title)
    (let* ([files (dir→files SOURCE-DIR)]
           [lo-lines (map file->lines files)]
           [lo-struct (map (lines→data lod get-default-struct) lo-lines)]
           [lo-table (lo-struct→tables lod lo-struct)]
           [lo-html (map ((table→html HOME-PAGE STYLE-SOURCE title) lod) lo-table)] ; these back pages should be (get-root-file-name d)
           [lo-html (cons ((get-index-file HOME-PAGE HOME-PAGE STYLE-SOURCE title) lod) lo-html)]
           [lo-html (append lo-html (map (get-root-file "index.html" HOME-PAGE STYLE-SOURCE title) (filter (λ (d) (data-table? d)) lod)))])
      (map (html→disk OUT-DIR) lo-html))))
