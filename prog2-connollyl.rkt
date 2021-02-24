;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prog2-connollyl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1
;; roots : number X number X number -> list(number)
;; Calculate the quadratic formula given 3 numbers: a, b, c
;; The result is a list of two if roots are different; one if the same.
(define (roots a b c)
  (let ((d (- (* b b) (* 4 a c))))
    (if (zero? d)
      (list(/ (+ (- b) (sqrt d)) (* 2 a)))
      (list(/ (+ (- b) (sqrt d)) (* 2 a)) (/ (- (- b) (sqrt d)) (* 2 a))))))

;; Problem 2
;; future-date : year X month X day X num-day -> list(year month day)
;; Calculates the year, month, and day that is num-days from the given
;; date. The result is a list of the future date in the form of (year month day)
(define (future-date year month day num-day)
  (if (> num-day 0)    
      (if (equal? month 2)
          (if (cal-leap-year year)
              (if (< day 29)
                  (future-date year month (add1 day) (sub1 num-day))
                  (future-date year (add1 month) 1 (sub1 num-day)))
              (if (< day 28)
                  (future-date year month (add1 day) (sub1 num-day))
                  (future-date year (add1 month) 1 (sub1 num-day))))
          (if (equal? month 12)
              (if (< day 31)
                  (future-date year month (add1 day) (sub1 num-day))
                  (future-date (add1 year) 1 1 (sub1 num-day)))
              (if (equal? (remainder month 2) 0)
                  (if (< day 30)
                      (future-date year month (add1 day) (sub1 num-day))
                      (future-date year (add1 month) 1 (sub1 num-day)))
                  (if (< day 31)
                      (future-date year month (add1 day) (sub1 num-day))
                      (future-date year (add1 month) 1 (sub1 num-day))))))
      (list year month day)))

;; cal-leap-year : year -> true if it is a leap year
;; Will return true if the given year is a leap year
(define (cal-leap-year year)
  (if (equal? (remainder year 4) 0)
      (if (not (equal? (remainder year 100) 0))
          (equal? 0 0)
          (if (equal? (remainder year 400) 0)
              (equal? 0 0)
              (equal? 1 0)))
      (equal? 1 0)))

;; Problem 3
;; flat : any list -> list containing only atomic items
;; Will take any list and return a list containing all
;; the atomic items in the orignal list
(define (flat lst) 
  (if (empty? lst)
      empty
      (if (atom? (first lst))
          (append (list (first lst)) (flat (rest lst)))
          (append (flat (first lst)) (flat (rest lst))))))
  
;; Atom? : x -> true if x is an atomic item
;; Will take an item and return true if the
;; item is an atom, false otherwise
(define(atom? x)
  (if (not (or (empty? x) (or (string? x) (cons? x))))
      true
      false))












       