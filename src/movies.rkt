#lang racket

(require "generate-tables.rkt"
         "constants.rkt"
         "utilities.rkt"
         racket/struct)


(struct movie
  (title
   director
   writers
   year
   genres
   rating
   opinion)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (m) 'movie)
      (λ (m) (list (movie-title m)
                   (movie-director m)
                   (movie-writers m)
                   (movie-year m)
                   (movie-genres m)
                   (movie-rating m)))))])

(define generic-sort-movie
  (λ (m1 m2)
    (if (< (movie-year m1) (movie-year m2))
        #t
        (if (< (movie-year m2) (movie-year m1))
            #f
            (string<=? (movie-director m1) (movie-director m2))))))

(define MOVIE-TITLE
  (data
   "Title"
   (λ (m) (movie-title m))
   (λ (val m)
     (movie
      val
      (movie-director m)
      (movie-writers m)
      (movie-year m)
      (movie-genres m)
      (movie-rating m)
      (movie-opinion m)))
   (λ (m) (movie-title m))
   #t
   ALPHABET
   (λ (portion)
     (λ (m)
       (equal? portion (char-upcase (string-car (movie-title m))))))
   string
   (λ (m1 m2)
     (if (string<=? (movie-title m1) (movie-title m2))
        #t
        (if (string<=? (movie-title m2) (movie-title m1))
            #f
            (string<=? (movie-director m1) (movie-director m2)))))
   (λ (m)
     (not (char-alphabetic? (string-car (movie-title m)))))))

(define MOVIE-DIRECTOR
  (data
   "Director"
   (λ (m) (movie-director m))
   (λ (val m)
     (movie
      (movie-title m)
      val
      (movie-writers m)
      (movie-year m)
      (movie-genres m)
      (movie-rating m)
      (movie-opinion m)))
   (λ (m) (movie-director m))
   #t
   ALPHABET
   (λ (portion)
     (λ (m)
       (equal? portion (char-upcase (string-car (movie-director m))))))
   string
   (λ (m1 m2)
    (if (string<=? (movie-director m1) (movie-director m2))
        #t
        (if (string<=? (movie-director m2) (movie-director m1))
            #f
            (< (movie-year m1) (movie-year m2)))))
   #f))

(define MOVIE-RATING
  (data
   "Rating"
   (λ (m) (movie-rating m))
   (λ (val m)
     (movie
      (movie-title m)
      (movie-director m)
      (movie-writers m)
      (movie-year m)
      (movie-genres m)
      (string->number val)
      (movie-opinion m)))
   (λ (m) (number->string (movie-rating m)))
   #t
   (list 1 2 3 4 5 6 7 8 9 10)
   (λ (portion)
     (λ (m)
       (equal? portion (movie-rating m))))
   number->string
   generic-sort-movie
   #f))

(define MOVIE-YEAR
  (data
   "Year"
   (λ (m) (movie-year m))
   (λ (val m)
     (movie
      (movie-title m)
      (movie-director m)
      (movie-writers m)
      (string->number val)
      (movie-genres m)
      (movie-rating m)
      (movie-opinion m)))
   (λ (m) (number->string (movie-year m)))
   #t
   (list 1950 1960 1970 1980 1990 2000 2010 2020)
   (λ (portion)
     (λ (m)
       (let ([yr (movie-year m)])
         (and
          (<= portion yr)
          (> (+ portion 10) yr)))))
   number->string
   (λ (m1 m2)
     (let ([m1-yr (movie-year m1)]
           [m2-yr (movie-year m2)])
       (if (< m1-yr m2-yr)
           #t
           (if (> m1-yr m2-yr)
               #f
               (string<=? (movie-director m1) (movie-director m2))))))
   #f))

(define MOVIE-WRITERS
  (data
   "Writers"
   (λ (m) (movie-writers m))
   (λ (val m)
     (movie
      (movie-title m)
      (movie-director m)
      (string-split val ",")
      (movie-year m)
      (movie-genres m)
      (movie-rating m)
      (movie-opinion m)))
   (λ (m) (los→str (movie-writers m)))
   #t
   (list "Boris Strugatsky" "Arkady Strugatsky" "Andrei Tarkovsky" "Barry Gifford" "Satoshi Kon" "Sadayuki Murai" "Tonino Guerra")
   (λ (portion)
     (λ (m)
       (member portion (movie-writers m))))
   id
   generic-sort-movie
   #f))

(define MOVIE-GENRES
  (data
   "Genres"
   (λ (m) (movie-genres m))
   (λ (val m)
     (movie
      (movie-title m)
      (movie-director m)
      (movie-writers m)
      (movie-year m)
      (string-split val ",")
      (movie-rating m)
      (movie-opinion m)))
   (λ (m) (los→str (movie-genres m)))
   #t
   (list "Animation" "Art" "Comedy" "Crime" "Drama" "Fantasy" "Horror" "Mystery" "Psychological Thriller" "Romance" "Science Fiction" "Thriller")
   (λ (portion)
     (λ (m)
       (member portion (movie-genres m))))
   id
   generic-sort-movie
   #f))

(define MOVIE-OPINION
  (data
   "Opinion"
   (λ (m) (movie-opinion m))
   (λ (val m)
     (movie
      (movie-title m)
      (movie-director m)
      (movie-writers m)
      (movie-year m)
      (movie-genres m)
      (movie-rating m)
      val))
   (λ (m) (movie-opinion m))
   #f
   #f
   #f
   #f
   #f
   #f
   ))

;; --------
(define build-movie
  (λ ()
    (go "Movies"
        (list MOVIE-TITLE MOVIE-DIRECTOR MOVIE-WRITERS MOVIE-YEAR MOVIE-GENRES MOVIE-RATING MOVIE-OPINION)
        (λ () (movie #f #f #f #f #f #f #f))
        "Movies")))

(build-movie)
