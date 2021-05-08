#lang racket

(require "generate-tables.rkt"
         "constants.rkt"
         "utilities.rkt"
         racket/struct)


(struct music
  (artist
   album
   year
   genres
   rating
   opinion)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (m) 'music)
      (λ (m) (list (music-artist m)
                   (music-album m)
                   (music-year m)
                   (music-genres m)
                   (music-rating m)))))])

(define generic-sort-music
  (λ (m1 m2)
    (if (string<=? (music-artist m1) (music-artist m2))
        #t
        (if (string<=? (music-artist m2) (music-artist m1))
            #f
            (< (music-year m1) (music-year m2))))))

(define MUSIC-ARTIST
  (data
   "Artist"
   (λ (m) (music-artist m))
   (λ (val m)
     (music
      val
      (music-album m)
      (music-year m)
      (music-genres m)
      (music-rating m)
      (music-opinion m)))
   (λ (m) (music-artist m))
   #t
   ALPHABET
   (λ (portion)
     (λ (m)
       (equal? portion (string-car (music-artist m)))))
   string
   generic-sort-music
   (λ (m)
     (not (char-alphabetic? (string-car (music-artist m)))))))

(define MUSIC-ALBUM
  (data
   "Album"
   (λ (m) (music-album m))
   (λ (val m)
     (music
      (music-artist m)
      val
      (music-year m)
      (music-genres m)
      (music-rating m)
      (music-opinion m)))
   (λ (m) (music-album m))
   #t
   ALPHABET
   (λ (portion)
     (λ (m)
       (equal? portion (string-car (music-album m)))))
   string
   (λ (m1 m2)
     (string<=? (music-album m1) (music-album m2)))
   (λ (m)
     (not (char-alphabetic? (string-car (music-album m)))))))

(define MUSIC-RATING
  (data
   "Rating"
   (λ (m) (music-rating m))
   (λ (val m)
     (music
      (music-artist m)
      (music-album m)
      (music-year m)
      (music-genres m)
      (string->number val)
      (music-opinion m)))
   (λ (m) (number->string (music-rating m)))
   #t
   (list 1 2 3 4 5 6 7 8 9 10)
   (λ (portion)
     (λ (m)
       (equal? portion (music-rating m))))
   number->string
   generic-sort-music
   #f))

(define MUSIC-YEAR
  (data
   "Year"
   (λ (m) (music-year m))
   (λ (val m)
     (music
      (music-artist m)
      (music-album m)
      (string->number val)
      (music-genres m)
      (music-rating m)
      (music-opinion m)))
   (λ (m) (number->string (music-year m)))
   #t
   (list 1950 1960 1970 1980 1990 2000 2010 2020)
   (λ (portion)
     (λ (m)
       (let ([yr (music-year m)])
         (and
          (<= portion yr)
          (> (+ portion 10) yr)))))
   number->string
   (λ (m1 m2)
     (let ([m1-yr (music-year m1)]
           [m2-yr (music-year m2)])
       (if (< m1-yr m2-yr)
           #t
           (if (> m1-yr m2-yr)
               #f
               (string<=? (music-artist m1) (music-artist m2))))))
   #f))

(define MUSIC-GENRES
  (data
   "Genres"
   (λ (m) (music-genres m))
   (λ (val m)
     (music
      (music-artist m)
      (music-album m)
      (music-year m)
      (string-split val ",")
      (music-rating m)
      (music-opinion m)))
   (λ (m) (los→str (music-genres m)))
   #t
   (list "Shoegaze" "Noise" "Industrial" "Jazz" "Art")
   (λ (portion)
     (λ (m)
       (member portion (music-genres m))))
   id
   generic-sort-music
   #f))

(define MUSIC-OPINION
  (data
   "Opinion"
   (λ (m) (music-opinion m))
   (λ (val m)
     (music
      (music-artist m)
      (music-album m)
      (music-year m)
      (music-genres m)
      (music-rating m)
      val))
   (λ (m) (music-opinion m))
   #f
   #f
   #f
   #f
   #f
   #f
   ))

;; --------
(define build-music
  (λ () (go (list MUSIC-ARTIST MUSIC-ALBUM MUSIC-YEAR MUSIC-GENRES MUSIC-RATING MUSIC-OPINION)
            (λ () (music #f #f #f #f #f #f)))))