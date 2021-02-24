;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prog3-connollyl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1
;; dot-product : list X list -> number
;; Calculates the dot product of two lists
;; given the lists are the same length. Returns
;; the dot product of the two lists.
(define (dot-product a b)
  (apply + (map * a b)))

;; Problem 2
;; home-wins : list -> number
;; Takes a list of NBA game results
;; and determines how many of the games
;; were home wins.
(define-struct results (home away))

(define (home-wins lst)
  (apply + (map (lambda (x)
                  (if (> (results-home x) (results-away x))
                      1
                      0))
                  lst)))

;; Problem 3
;; above-average : list -> list
;; Takes a list of integers and
;; returns a list of the integers
;; that are above the average of the
;; original list
(define (above-average lst)
  (filter (lambda (x) (> x (/ (apply + lst) (length lst)))) lst))

;; Problem 4
;; values-by-layers : tree -> list
;; Takes a list of nodes and
;; returns a list of the values
;; as a tree.
(define-struct node (value left right))

(define-struct tip ()) ;; empty node

(define (values-by-layers tree)
  (if (tip? tree)
      (list)
      (queue (list tree) null)))

;; queue: list X list -> list
;; Helper function for values-by-layers,
;; puts all node values in a queue and adds them
;; by layer to the result.
(define (queue lst result)
  (if (empty? lst)
      result
   (if (tip? (first lst))
     (queue (rest lst) result)
   (if (tip? (node-left (first lst)))
       (if (tip? (node-right (first lst)))
          (queue (rest lst) (append result (list (node-value (first lst)))))
         (queue (append (rest lst) (list (node-right (first lst))))
                        (append result (list (node-value (first lst))))))
       (queue (append (rest lst) (list (node-left (first lst)) (node-right (first lst))))
                        (append result (list (node-value (first lst)))))))))