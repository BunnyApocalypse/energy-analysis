#lang racket
(require csc151)
(require plot)
;;;
;;;TO DO
;;;
;1; DOCUMENT ALL PROCEDURES 
;;;
;2; ADD JANUARY AND JULY TICK MARKS TO GRAPHS 
;;;
;3; MAKE GRAPHS LOG SCALE
;;;

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
;;;   clean-reformatted-energy-data
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



;;; Procedure:
;;;    consolidate-gas-production
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
                       (cons (+ (car current-list) (caddr current-list)) new-list)
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

;;; Procedure:
;;;   create-popularity-ratio-table
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
(define create-popularity-ratio-table
  (lambda (consumption-table production-table)
    (let ([round-to-sig-figs (lambda (num figs)
                               (let ([fig-factor (reduce * (make-list figs 10))])
                                 (/ (round (* num fig-factor)) fig-factor)))]
          [my-consumption-table (reverse (cons (list 1) (cdr consumption-table)))]
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

(define popularity-ratio-table
  (create-popularity-ratio-table reformatted-consumption-data reformatted-production-data))

;;; Procedure:
;;;   remove-annual-totals
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
(define remove-annual-totals
  (lambda (lst)
    (if (= 13 (list-ref lst 1))
        #f
        #t)))

(define total-less-popularity-ratio-table
  (cons (car popularity-ratio-table) (filter (section remove-annual-totals <>) (cdr popularity-ratio-table))))



;;; Procedure:
;;;   make-popularity-ratio-data-points
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
(define make-popularity-ratio-data-points
  (lambda (table)
    (let* ([time-scale-metric (lambda (table)
                                (let ([my-table (reverse (cons (list 0 0) (cdr table)))])
                                  (let kernel ([table-remaining (cdr my-table)]
                                               [first-digit (cadar my-table)]
                                               [second-digit (caar my-table)]
                                               [metric null])
                                    (cond [(null? table-remaining)
                                           metric]
                                          [else
                                           (kernel (cdr table-remaining)
                                                   (cadar table-remaining)
                                                   (caar table-remaining)
                                                   (cons (+ first-digit (* 12 (- second-digit 1984))) metric))]))))]
           [time-values (reverse (cons 0 (time-scale-metric table)))]
           [my-table (reverse (cons (make-list 11 0) (map cddr (cdr table))))]
           [headers (map (o (section list "Timestep" <>) (section string-replace <>  " Popularity Ratio" "")) (cddar table))])
      (let kernel ([new-table null]
                   [table-remaining (cdr my-table)]
                   [new-list null]
                   [current-list (reverse (car my-table))]
                   [current-time-value (car time-values)]
                   [time-values-remaining (cdr time-values)])
        (cond [(null? table-remaining)
               (cons headers new-table)]
              [(null? current-list)
               (kernel (cons new-list new-table)
                       (cdr table-remaining)
                       null
                       (reverse (car table-remaining))
                       (car time-values-remaining)
                       (cdr time-values-remaining))]
              [else
               (kernel new-table
                       table-remaining
                       (cons (cons current-time-value (cons (car current-list) null)) new-list)
                       (cdr current-list)
                       current-time-value
                       time-values-remaining)])))))


(define total-less-popularity-ratio-data-points
  (make-popularity-ratio-data-points total-less-popularity-ratio-table))


(define get-data
  (lambda (table num)
    (map (section list-ref <> num) table)))

(define total-primary-energy-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 0))
(define total-renewable-energy-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 1))
(define biomass-energy-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 2))
(define wind-energy-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 3))
(define solar-energy-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 4))
(define geothermal-energy-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 5))
(define hydroelectric-power-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 6))
(define nuclear-electric-power-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 7))
(define total-fossil-fuels-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 8))
(define petroleum-to-crude-oil-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 9))
(define combined-natural-gas-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 10))
(define coal-popularity-ratio-data
  (get-data total-less-popularity-ratio-data-points 11))





(define carbon-energy-plot
  (plot (list
         (function (lambda (x) 1) #:color "purple" #:style 'dot #:width 0.75)
         (lines (cdr biomass-energy-popularity-ratio-data)  #:color "darkgreen" #:style 'long-dash #:label (cadar biomass-energy-popularity-ratio-data))
         ;(points (cdr biomass-energy-popularity-ratio-data) #:fill-color 2 #:sym 'fullcircle1)
         (lines (cdr total-fossil-fuels-popularity-ratio-data)  #:color "slategray" #:style 'dot-dash #:label (cadar total-fossil-fuels-popularity-ratio-data))
         ;(points (cdr total-fossil-fuels-popularity-ratio-data) #:fill-color 8 #:sym 'fullcircle1)
         (lines (cdr petroleum-to-crude-oil-popularity-ratio-data)  #:color "black" #:style 'short-dash #:label (cadar petroleum-to-crude-oil-popularity-ratio-data))
         ;(points (cdr petroleum-to-crude-oil-popularity-ratio-data) #:fill-color 9 #:sym 'fullcircle1)
         (lines (cdr combined-natural-gas-popularity-ratio-data) #:color "blue" #:style 'long-dash  #:label (cadar combined-natural-gas-popularity-ratio-data))
         ;(points (cdr combined-natural-gas-popularity-ratio-data) #:fill-color 10 #:sym 'fullcircle1)
         (lines (cdr coal-popularity-ratio-data) #:color "brown" #:style 'dot #:label (cadar coal-popularity-ratio-data))
         ;(points (cdr coal-popularity-ratio-data) #:fill-color 11 #:sym 'fullcircle1)
         )
        #:title "Popularity Ratios over Time for Carbon Sources of Energy"
        #:x-label "Time (Months from January 1984)"
        #:y-label "Ratio"
        #:x-min 0
        #:x-max 420
        #:y-min 0
        #:y-max 100;0000
        #:width 1000
        #:height 1000))

(define renewable-energy-plot
  (plot (list
         (function (lambda (x) 1) #:color "purple" #:style 'dot #:width 0.75)
         (lines (cdr total-renewable-energy-popularity-ratio-data)  #:color "darkgreen" #:style 'dot-dash #:width 1.3 #:label (cadar total-renewable-energy-popularity-ratio-data))
         ;(points (cdr total-renewable-energy-popularity-ratio-data) #:fill-color "darkgreen" #:sym 'fullcircle1)
         (lines (cdr wind-energy-popularity-ratio-data)  #:color "slategray" #:style 'dot #:width 1.3 #:label (cadar wind-energy-popularity-ratio-data))
         ;(points (cdr wind-energy-popularity-ratio-data) #:fill-color "slategray" #:sym 'fullcircle1)
         (lines (cdr solar-energy-popularity-ratio-data)  #:color "darkorange" #:style 'long-dash #:width 1.3 #:label (cadar solar-energy-popularity-ratio-data))
         ;(points (cdr solar-energy-popularity-ratio-data) #:fill-color "darkorange" #:sym 'fullcircle1)
         (lines (cdr geothermal-energy-popularity-ratio-data)  #:color "brown" #:style 'short-dash #:width 1.3 #:label (cadar geothermal-energy-popularity-ratio-data))
         ;(points (cdr geothermal-energy-popularity-ratio-data) #:fill-color "brown" #:sym 'fullcircle1)
         (lines (cdr hydroelectric-power-popularity-ratio-data)  #:color "darkblue" #:style 'dot #:width 1.3 #:label (cadar hydroelectric-power-popularity-ratio-data))
         ;(points (cdr hydroelectric-power-popularity-ratio-data) #:fill-color "darkblue" #:sym 'fullcircle1)
         (lines (cdr nuclear-electric-power-popularity-ratio-data)  #:color "red" #:style 'solid #:width 1.3 #:label (cadar nuclear-electric-power-popularity-ratio-data))
         ;(points (cdr nuclear-electric-power-popularity-ratio-data) #:fill-color "red" #:sym 'fullcircle1)
         )
        #:title "Popularity Ratios over Time for Renewable Sources of Energy"
        #:x-label "Time (Months from January 1984)"
        #:y-label "Ratio"
        #:x-min 0
        #:x-max 420
        #:y-min 0
        #:y-max 20
        #:width 1000
        #:height 1000))

(define total-primary-energy-plot
  (plot (list
         (function (lambda (x) 1) #:color "purple" #:style 'dot #:width 0.75)
         (lines (cdr total-primary-energy-popularity-ratio-data) #:color 0 #:style 'solid #:label (cadar total-primary-energy-popularity-ratio-data))
         ;(points (cdr total-primary-energy-popularity-ratio-data) #:fill-color 0  #:sym 'fullcircle1)
         )
        #:title "Popularity Ratio over Time for Total Primary Energy"
        #:x-label "Time (Months from January 1984)"
        #:y-label "Ratio"
        #:x-min 0
        #:x-max 420
        #:y-min 0
        #:y-max 50
        #:width 1000
        #:height 1000))



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



