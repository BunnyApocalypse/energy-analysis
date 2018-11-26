#lang racket
(require csc151)
(define energy-production
  (read-csv-file "/Users/BigO/Desktop/College/Classes/Semester 5 Fall 2018/Csc 151/Project/MER_T01_02.csv"))

(define header (append (list "Year" "Month") (cdr (map car (tally-all (map (section list-ref <> 4) energy-production))))))

(define my-sorting-predicate
  (lambda (str1 str2)
    (let ([yr1 (string->number (substring str1 0 4))]
          [yr2 (string->number (substring str2 0 4))])
      (< yr1 yr2))))


(define list-of-dates
  (map car (tally-all (map (section list-ref <> 1) energy-production))))

(define reorganize-data
  (lambda (table dates)
    (let ([new-dates (reverse dates)])
      (let kernel ([table-so-far null]
                   [remaining new-dates])
        (cond [(null? remaining)
               table-so-far]
              [else
               (kernel (cons (filter (o (section equal? (car remaining) <>) cadr) table) table-so-far)
                       (cdr remaining))])))))

(define reorganized-energy-production
  (reorganize-data energy-production list-of-dates))


(define clean-data
  (lambda (data)
    (letrec ([kernel
              (lambda (data index max-index)
                (cond
                 [(> index max-index)
                  data]
                 [else
                  (kernel (filter (o (section (negate equal?) "Not Available" <>)
                                     (section list-ref <> index)) data) (+ index 1) max-index)]))])
      (kernel data 0 (length (car data))))))
#|
(define reformat-data
  (lambda (lst)
    (let ([year (string->number (substring (list-ref lst 1) 0 4))]
          [month (string->number (substring (list-ref lst 1) 4 6))])
      (list (c
      
    
    (list (substring (list-ref lst 1))
|#