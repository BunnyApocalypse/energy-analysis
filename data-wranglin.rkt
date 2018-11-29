#lang racket
(require csc151)
(define energy-production-data
  (read-csv-file "MER_T01_02.csv"))

(define energy-consumption-data
  (read-csv-file "MER_T01_03.csv"))

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
              [(= 1 counter)
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
            [(string->number (car lst-remaining))
             (kernel clean-tbl
                     tbl-remaining
                     (cons (car lst-remaining) clean-lst)
                     (cdr lst-remaining))]
            [else
             (kernel clean-tbl
                     (cdr tbl-remaining)
                     null
                     (reverse (car tbl-remaining)))]))))
