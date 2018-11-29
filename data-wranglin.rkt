#lang racket
(require csc151)
(define energy-production-data
  (read-csv-file "/Users/BigO/Desktop/College/Classes/Semester 5 Fall 2018/Csc 151/Project/MER_T01_02.csv"))

(define energy-consumption-data
  (read-csv-file "/Users/BigO/Desktop/College/Classes/Semester 5 Fall 2018/Csc 151/Project/MER_T01_03.csv"))
             
;;; Procedure:
;;;   reformat-energy-data
;;; Parameters:
;;;   table, a table in
;;; Purpose:
;;;   
;;; Produces:
;;;   
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   
(define reformat-energy-data
  (lambda (table)
    (let* ([my-> (lambda (lst1 lst2)
                   (string-ci>? (cadr lst1)
                                (cadr lst2)))]
           [my-table (sort (cdr table) my->)]
           [list-o-categories (reverse (append (map car (tally-all (map (section list-ref <> 4) my-table))) (list "Month" "Year")))]
           [number-o-categories (length (cddr list-o-categories))]
           [get-month-and-year (lambda (tbl) (list (substring (cadar tbl) 0 4) (substring (cadar tbl) 4 6)))])
      (let kernel ([tbl-so-far null]
                   [tbl-remaining my-table]
                   [current-row null]
                   [counter number-o-categories])
        (cond [(null? tbl-remaining)
               (cons list-o-categories tbl-so-far)]
              [(= 0 (decrement counter))
               (kernel
                (cons (append (get-month-and-year tbl-remaining) (cons (caddar tbl-remaining) current-row)) tbl-so-far)
                (cdr tbl-remaining)
                null
                number-o-categories)]
              [else
               (kernel tbl-so-far
                       (cdr tbl-remaining)
                       (cons (caddar tbl-remaining) current-row)
                       (decrement counter))])))))

(define reformatted-production-data
  (reformat-energy-data energy-production-data))
(define reformatted-consumption-data
  (reformat-energy-data energy-consumption-data))

;taken from Olek's and Quang's solution to Problem 5b of Assignment 6
;;; Procedure:
;;;   
;;; Parameters:
;;;   
;;; Purpose:
;;;   
;;; Produces:
;;;   
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   
(define full-power-of-num
  (lambda (y x)
    (cond [(= x 0)
           1]
          [(> x 0)
           (* y (full-power-of-num y (- x 1)))]
          [else
           (/ (full-power-of-num y (+ x 1)) y)])))


;;; Procedure:
;;;   
;;; Parameters:
;;;   
;;; Purpose:
;;;   
;;; Produces:
;;;   
;;; Preconditions:
;;;   
;;; Postconditions:
;;;   
(define clean-reformatted-energy-data
 (lambda (table)
   (let kernel ([clean-tbl null]
                [tbl-remaining (reverse (cdr table))]
                [clean-lst null]
                [lst-remaining (reverse (car table))])
     (cond  [(null? tbl-remaining)
             (cons (car table) clean-tbl)]
            [(null? lst-remaining)
             (kernel (cons clean-lst clean-tbl)
                     (cdr tbl-remaining)
                     null
                     (reverse (car tbl-remaining)))]
            [(and #t (string->number (car lst-remaining)))
             (kernel clean-tbl
                     tbl-remaining
                     (cons (car lst-remaining) clean-lst)
                     (cdr lst-remaining))]
            [else
             (kernel clean-tbl
                     (cdr tbl-remaining)
                     null
                     (reverse (car tbl-remaining)))]))))


;for testing                               
    #|
(define my-<
  (lambda (lst1 lst2)
    (if (string-ci<? (cadr lst1)
                     (cadr lst2))
        #t
        #f)))
(define my->
  (lambda (lst1 lst2)
    (if (string-ci>? (cadr lst1)
                     (cadr lst2))
        #t
        #f)))
(define not-a-number
  (lambda (lst)
    (if (and #t (string->number (cadr lst)))
        #f
        #t)))
(define test-table
  (list (list "hello" 1 's) (list "is" 4 's) (list "name" 3 's) (list "my" 2 's) (list "Bob" 5 's)))
|#


;junk
#|
(define reformat-energy-data
  (lambda (table)
    (let* ([list-o-categories (append (list "Year" "Month") (cdr (map car (tally-all (map (section list-ref <> 4) table)))))]
           [list-o-dates (map car (tally-all (map (section list-ref <> 1) table)))]
           [data-sorter  (lambda (tbl dts)
                           (let ([new-dates (reverse dts)])
                             (let kernel ([table-so-far null]
                                          [remaining new-dates])
                               (cond [(null? remaining)
                                      table-so-far]
                                     [else
                                      (kernel (cons (filter (o (section equal? (car remaining) <>) cadr) tbl) table-so-far)
                                              (cdr remaining))]))))]
           [section-formatter (lambda (lst) (let ([year (substring (list-ref (car lst) 1) 0 4)]
                                                  [month (substring (list-ref (car lst) 1) 4 6)]
                                                  [vals (map (section list-ref <> 2) lst)])
                                              (cons year (cons month vals))))])
      (cons list-o-categories (cdr  (map section-formatter (data-sorter table list-o-dates)))))))

NEED TO FIX THE DATA CLEANER
(define reformatted-energy-data-cleaner
  (lambda (table)
    (let ([if-val-in-list-null-else-list (lambda (val lst)
                                           (let kernel ([list-so-far null]
                                                        [remaining lst])
                                             (cond [(null? remaining)
                                                    list-so-far]
                                                   [(equal? val (car remaining))
                                                    null]
                                                   [else
                                                    (kernel (cons (car lst) list-so-far) (cdr remaining))])))]
          [cleaner (lambda (tbl)
                     (let ([my-table (reverse (cdr table))])
                       (let kernel ([table-so-far null]
                                    [remaining my-table])
                         (cond [(null? remaining)
                                table-so-far]
                               [(null? (car remaining))
                                (kernel table-so-far (cdr remaining))]
                               [else
                                (kernel (cons (car remaining) table-so-far)
                                        (cdr remaining))]))))])
      (cons (car table) (cleaner (map (section if-val-in-list-null-else-list "Not Available" <>) table))))))
                           

                     
(define reformatted-energy-production-data
  (reformat-energy-data energy-production-data))

(define reformatted-energy-consumption-data
  (reformat-energy-data energy-consumption-data))

(define cleaned-reformatted-energy-production-data
  (reformatted-energy-data-cleaner reformatted-energy-production-data))

(define cleaned-reformatted-energy-consumption-data
  (reformatted-energy-data-cleaner reformatted-energy-consumption-data))

(define header (append (list "Year" "Month") (cdr (map car (tally-all (map (section list-ref <> 4) energy-production-data))))))

(define list-of-categories
  (lambda (table)
    (append (list "Year" "Month") (cdr (map car (tally-all (map (section list-ref <> 4) table)))))))

(define list-of-production-categories
  (list-of-categories energy-production-data))
(define list-of-consumption-categories
  (list-of-categories energy-consumption-data))

(define list-of-dates
  (lambda (table)
    (map car (tally-all (map (section list-ref <> 1) table)))))

(define list-of-production-dates
  (list-of-dates energy-production-data))

(define list-of-consumption-dates
  (list-of-dates energy-consumption-data))
  

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
  (reorganize-data energy-production-data list-of-production-dates))
(define reorganized-energy-consumption
  (reorganize-data energy-consumption-data list-of-consumption-dates))



(define reformat-data
  (lambda (lst)
    (let ([year (substring (list-ref (car lst) 1) 0 4)]
          [month (substring (list-ref (car lst) 1) 4 6)]
          [vals (map (section list-ref <> 2) lst)])
      (cons year (cons month vals)))))

(define reformatted-production-data
  (cons list-of-production-categories (cdr (map reformat-data reorganized-energy-production))))
(define reformatted-consumption-data
  (cons list-of-consumption-categories (cdr (map reformat-data reorganized-energy-consumption))))

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
|#
