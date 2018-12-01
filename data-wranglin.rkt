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
;FIXME wasted work in working-row definitions
(define create-ratio-table
  (lambda (usage-table production-table)
    (let kernel ([new-table null]
                 [tbl1 (reverse (cdr usage-table))]
                 [tbl2 (reverse (cdr production-table))]
                 [working-row-1 (car (reverse (cdr usage-table)))]
                 [working-row-2 (car (reverse (cdr production-table)))]
                 [counter 0]
                 [row null])
      (cond
        [(or (null? tbl1) (null? tbl2))
            new-table]
        [(and (or (= 1 (length tbl1)) (= 1 (length tbl2))) (= counter 13))
         (kernel
          (cons (reverse row) new-table)
          (cdr tbl1)
          (cdr tbl2)
          (car tbl1)
          (car tbl2)
          0
          null)]
        [(= counter 13)
         (kernel
          (cons (reverse row) new-table)
          (cdr tbl1)
          (cdr tbl2)
          (car (cdr tbl1))
          (car (cdr tbl2))
          0
          null)]
        [(= counter 0)
         (kernel
          new-table
          tbl1
          tbl2
          (cddr working-row-1)
          (cddr working-row-2)
          (+ counter 1)
          (cons (cadr working-row-1) (cons (car working-row-1) row)))]
        [else
         (kernel
          new-table
          tbl1
          tbl2
          (cdr working-row-1)
          (cdr working-row-2)
          (+ counter 1)
          (cons (if (equal? "0" (car working-row-2))
                    "Production was 0"
                    (/ (string->number (car working-row-1)) (string->number (car working-row-2)))) row))]))))

(define reformatted-production-data
  (clean-reformatted-energy-data (reformat-energy-data energy-production-data)))
(define reformatted-consumption-data
  (clean-reformatted-energy-data (reformat-energy-data energy-consumption-data)))
(define test (create-ratio-table reformatted-consumption-data reformatted-production-data))

