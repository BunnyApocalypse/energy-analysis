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

(define reformatted-energy-production-data
  (reformat-energy-data energy-production-data))
(define reformatted-energy-consumption-data
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
             [(< 0 (or (string->number (car lst-remaining)) -1))
              (kernel clean-tbl
                      tbl-remaining
                      (cons (string->number (car lst-remaining)) clean-lst)
                      (cdr lst-remaining))]
             [else
              (kernel clean-tbl
                      (cdr tbl-remaining)
                      null
                      (reverse (car tbl-remaining)))]))))

(define cleaned-reformatted-energy-production-data
  (clean-reformatted-energy-data reformatted-energy-production-data))
(define cleaned-reformatted-energy-consumption-data
  (clean-reformatted-energy-data reformatted-energy-consumption-data))

(define round-to-sig-figs
  (lambda (num figs)
    (let ([fig-factor (reduce * (make-list figs 10))])
      (/ (round (* num fig-factor)) fig-factor))))


(define consolidate-gas-production
  (lambda (production-table)
    (let ([my-production-table (reverse (cons (list 1) (cdr production-table)))])
      (let kernel ([new-table null]
                   [tbl-remaining (cdr my-production-table)]
                   [new-list null]
                   [current-list (reverse (car my-production-table))]
                   [counter 0])
        (cond [(null? tbl-remaining)
               (cons (append (take (car production-table) 11)
                             (list "Crude Oil Production" "Combined (Dry & Liquid) Natural Gas Production" "Coal Production"))
                     new-table)]
              [(null? current-list)
               (kernel (cons new-list new-table)
                       (cdr tbl-remaining)
                       null
                       (reverse (car tbl-remaining))
                       0)]
              [(= 1 counter)
               (kernel new-table
                       tbl-remaining
                       (cons (round-to-sig-figs (+ (car current-list) (caddr current-list)) 6) new-list)
                       (cdr current-list)
                       2)]
              [(= 3 counter)
               (kernel new-table
                       tbl-remaining
                       new-list
                       (cdr current-list)
                       4)]
              [else
               (kernel new-table
                       tbl-remaining
                       (cons (car current-list) new-list)
                       (cdr current-list)
                       (increment counter))])))))


(define create-popularity-ratio-table
  (lambda (consumption-table production-table)
    (let ([my-consumption-table (reverse (cons (list 1) (cdr consumption-table)))]
          [my-production-table (reverse (cons (list 1) (cdr production-table)))]
          [new-header (list "Year" "Month" "Total Primary Energy Popularity Ratio"
                            "Total Renewable Energy Popularity Ratio" "Biomass Energy Popularity Ratio"
                            "Wind Energy Popularity Ratio" "Solar Energy Popularity Ratio"
                            "Geothermal Energy Popularity Ratio" "Hyrdroelectric Power Popularity Ratio"
                            "Nuclear Electric Power Popularity Ratio" "Total Fossil Fuels Popularity Ratio"
                            "Petroleum Consumption:Crude Oil Production*" "Combined Natural Gas Popularity Ratio*"
                            "Coal Popularity Ratio")])
      (let kernel ([new-table null]
                   [consumption-tbl-remaining (cdr my-consumption-table)]
                   [production-tbl-remaining (cdr my-production-table)]
                   [consumption-working-row (reverse (car my-consumption-table))]
                   [production-working-row (reverse (car my-production-table))]
                   [counter 0]
                   [new-row null])
        (cond [(and (null? consumption-tbl-remaining) (null? production-tbl-remaining))
               (cons new-header new-table)]
              [(= counter 12)
               (kernel (cons (cons (cadr consumption-working-row)
                                   (cons (car consumption-working-row) new-row))
                             new-table)
                       (cdr consumption-tbl-remaining)
                       (cdr production-tbl-remaining)
                       (reverse (car consumption-tbl-remaining))
                       (reverse (car production-tbl-remaining))
                       0
                       null)]
              [else
               (kernel new-table
                       consumption-tbl-remaining
                       production-tbl-remaining
                       (cdr consumption-working-row)
                       (cdr production-working-row)
                       (increment counter)
                       (cons (round-to-sig-figs (/ (car consumption-working-row)
                                                   (car production-working-row)) 6)
                             new-row))])))))



(define reformatted-production-data
  ;(consolidate-gas-production
   cleaned-reformatted-energy-production-data)

(define reformatted-consumption-data
  cleaned-reformatted-energy-consumption-data)

(define popularity-ratio-tbl
  (create-popularity-ratio-table reformatted-consumption-data reformatted-production-data))

;you can use these procedures to test the results of our popularity ratio table
(define cons-tst-lst
  (lambda (num max)
   (list-ref (take (cdr reformatted-consumption-data) max) num)))

(define prod-tst-lst
  (lambda (num max)
   (list-ref (take (cdr reformatted-production-data) max) num)))

(define list-divide
  (lambda (lst1 lst2)
    (map / (cddr lst1) (cddr lst2))))



#|
  (1984 1 1.3058098631470068 1.0 1.0 "Production was 0" 1.0 1.0 1.0 1.0 1.3582287005335538 1.7618493607754988 1.270653675804298 1.0378518129550782)
  (1984 2 1.180052608387967 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.2099185299227448 1.6141025048386415 1.2084325491890666 0.8381185795419854)
  (1984 3 1.1556963819864743 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.180717528551824 1.725146997706528 1.1034521271432531 0.7818139766878721)
  (1984 4 1.1115358993384157 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.1286594869929656 1.6377808098009283 0.9798682861424403 0.7941446848266912))
|#
