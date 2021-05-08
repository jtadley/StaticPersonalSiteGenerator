#lang racket

(require "utilities.rkt"
         "constants.rkt"
         racket/struct)

(provide (all-defined-out))

;; --------------

(define SOURCE-DIR "../SiteSource/Music")
(define BACK-PAGE "index.html")
(define HOME-PAGE "../../index.html")
(define STYLE-SOURCE "../../styles.css")
(define SEPARATOR ":")

;; --------------

(struct data
  (name        ; title (ex: "Artist")
   accessor    ; get the value for this data from a struct
   setter      ; takes a value and a struct and sets the name element
   to_string   ; get the value as a printable string
   table?      ; should we generate a table sorted by this
   ;; info for generating tables
   portions    ; #f OR list of things we are splitting the data into
   in_portion? ; #f OR predicate that takes in an element and says whether it belongs to a portion
   sort_fn     ; #f OR takes two elements of a struct and compares them
   overflow?   ; #f OR predicate that takes in an element and says whether it doesn't match a portion
   )
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (d) 'data)
      (λ (d) (list (data-name d)
                   (data-table? d)))))])

(define build-tables
  (λ (d)
    (let ([d-name (data-name d)]
          [d-portions (data-portions d)]
          [d-in_portion? (data-in_portion? d)]
          [d-sort_fn (data-sort_fn d)]
          [d-overflow? (data-overflow? d)])
      (letrec ([helper
                (λ (ls)
                  (cond
                    [(empty? ls)
                     (if d-overflow?
                         (begin
                           (build-table
                            d
                            (build-file-name d-name (car ls))
                            d-overflow?
                            d-sort_fn))
                         (void))
                     (writeln (string-append "done building table for: " d-name))]
                    [else (build-table
                           d
                           (build-file-name d-name (car ls))
                           d-in_portion?
                           d-sort_fn)
                          (helper (cdr ls))]))])
        (helper d-portions)))))

(define build-table
  (λ (d out-file filter-pred sort-fn)
    'todo))

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

(define go
  (λ (lod get-default-struct)
    (let* ([files (dir→files SOURCE-DIR)]
           [lo-lines (map file->lines files)]
           [lo-struct (map (lines→data lod get-default-struct) lo-lines)]
           [_ (writeln lo-struct)]
           [lo-tr (map (struct→tr lod) lo-struct)])
      lo-tr)))
