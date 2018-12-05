#lang racket

(require csc151)
(require plot)

;;; Here, we read our csv files containing our data, downloaded from the eia:
;;;    https://www.eia.gov/totalenergy/data/annual/
;;;    In "Annual Energy Review", under "Energy Overview", sections 1.2 and 1.3,
;;;    you will find the following datasets: Primary energy production by source
;;;                                          Primary energy consumption by source
;=======================================================================================================================
;0;RAW DATA
;==============================
(define energy-production-data
  (read-csv-file "MER_T01_02.csv"))

(define energy-consumption-data
  (read-csv-file "MER_T01_03.csv"))


;=======================================================================================================================
;1;REFORMAT  
;==============================
;;; Procedure:
;;;   reformat-energy-data
;;; Parameters:
;;;   table, a table that of the form of energy-production-data or energy-consumption-data
;;; Purpose:
;;;   Reformat the table into a more useful format.
;;; Produces:
;;;   reformatted-table, a table
;;; Preconditions:
;;;   table must have rows of the form '(val "MMMMYY" "Value" "Column_Order" "Description" val)
;;;        val is an arbitrary Scheme value.
;;;        "MMMMYY" is a string that can be converted into an integer, with the first four
;;;             digits of that integer indicating a year (can range from 0000 to 9999),
;;;             and the last two digits of that integer indicating a month (can range from
;;;             01 to 13).
;;;         "Value" is a string.
;;;         "Column_Order" is a string that can be converted into an integer, ranging from 1
;;;             to infinite, that indicates the column in reformatted-table that "Value" will
;;;             be placed in after the "Year" and "Month" columns.
;;;         "Description" is a string that indicates what quantity "Value" represents.
;;;   table must be sorted first by "Column_Order" and then by "MMMMYY", both in increasing order.
;;; Postconditions:
;;;   reformatted-table will be a list of lists with the first list being a header listing all
;;;         of the different types of datapoints available in the table, followed by individual
;;;         lists of given year-month combination's values for all points listed in the header.
;;;         These year-month combinations will increase in chronological order and cover all
;;;         dates available in the data. 
;;;   The number of columns in reformatted-table is 2 more than the maximum value of "Column_Order"
;;;         in table.
;;;   
(define reformat-energy-data
  (lambda (table)
    (let* (;This predicate determines if a "MMMMYY" in table is at a later date than some other "MMMMYY"
           [later-date? (lambda (lst1 lst2)
                          (string-ci>? (cadr lst1)
                                       (cadr lst2)))]
           ;The working table is sorted by the above predicate.
           [working-table (sort (cdr table) later-date?)]
           ;This is a header list that takes the categories from table.
           [list-o-categories (reverse (append (map car (tally-all (map (section list-ref <> 4) working-table))) (list "Month" "Year")))]
           [starting-column (length (cddr list-o-categories))]
           ;This procedure gets the month as a string from the first row in tbl.
           [get-month (lambda (tbl) (substring (cadar tbl) 4 6))]
           ;This procedure gets the year as a string from the first row in tbl.
           [get-year (lambda (tbl) (substring (cadar tbl) 0 4))])
      (let kernel ([reformatted-table null]
                   [table-remaining working-table]
                   [new-row null]
                   [current-column starting-column])
        (cond [(null? table-remaining)
               (cons list-o-categories reformatted-table)]
              [(= 1 current-column)
               (kernel
                (cons (cons (get-year table-remaining)
                            (cons (get-month table-remaining)
                                  ;we cons the first row of table-remaining without its first two entries onto
                                  ;the current row. This allows us to put the month and year in new-row
                                  ;as separate values. Our new row is ready to be cons-ed to the new table.
                                  (cons (caddar table-remaining)
                                        new-row)))
                      reformatted-table)
                ;we move to the next set of row in the table, and reset the other parameters.
                (cdr table-remaining)
                null
                starting-column)]
              [else
               (kernel reformatted-table
                       ;we add the "Value" entry to new-row and then recurse onto the next row in table. 
                       (cdr table-remaining)
                       (cons (caddar table-remaining) new-row)
                       (decrement current-column))])))))

;(define reformatted-energy-production-data
;  (reformat-energy-data energy-production-data))
;(define reformatted-energy-consumption-data
;  (reformat-energy-data energy-consumption-data))


;=======================================================================================================================
;2;CLEAN 
;==============================
;;; Procedure:
;;;   clean-reformatted-energy-data
;;; Parameters:
;;;   reformatted-data, a table
;;; Purpose:
;;;   to remove unwanted data from a reformatted US energy dataset
;;; Produces:
;;;   cleaned-reformatted-data, a table
;;; Preconditions:
;;;   reformatted-data must be an output of the procedure "reformat-energy-data"
;;; Postconditions:
;;;   cleaned-reformatted-data will be identical to reformatted-data but with all
;;;   rows containing non-number and less than or equal to 0 data removed besides the header.
(define clean-reformatted-energy-data
  (lambda (table)
    (let ([starting-column (- (length (car table)) 1)])
      (let kernel ([clean-table null]
                   [table-remaining (reverse (cdr table))]
                   [clean-row null]
                   [lst-remaining (reverse (car table))]
                   [current-column starting-column])
        (cond  [(null? table-remaining)
                (cons (car table) clean-table)]
               [(null? lst-remaining)
                (kernel (cons clean-row clean-table)
                        (cdr table-remaining)
                        null
                        (reverse (car table-remaining))
                        starting-column)]
               [(= current-column 1)
                (if (= 13 (string->number (car lst-remaining)))
                    (kernel clean-table
                            (cdr table-remaining)
                            null
                            (reverse (car table-remaining))
                            (- current-column 1))
                    (kernel clean-table
                            table-remaining
                            (cons (string->number (car lst-remaining)) clean-row)
                            (cdr lst-remaining)
                            (- current-column 1)))]
               [(< 0 (or (string->number (car lst-remaining)) -1))
                (kernel clean-table
                        table-remaining
                        (cons (string->number (car lst-remaining)) clean-row)
                        (cdr lst-remaining)
                        (- current-column 1))]
               [else
                (kernel clean-table
                        (cdr table-remaining)
                        null
                        (reverse (car table-remaining))
                        (- current-column 1))])))))

;(define cleaned-reformatted-energy-production-data
; (clean-reformatted-energy-data reformatted-energy-production-data))
;(define cleaned-reformatted-energy-consumption-data
;  (clean-reformatted-energy-data reformatted-energy-consumption-data))



;=======================================================================================================================
;3;CONSOLIDATE
;==============================

;;; Procedure:
;;;    consolidate-production-hydrocarbons
;;; Parameters:
;;;    production-table, a table
;;; Purpose:
;;;    to combine the dry and liquid natural gas and crude oil statistics from a production table
;;; Produces:
;;;    consolidated-production-table, a table
;;; Preconditions:
;;;    production-table must be a production table that has been put through the
;;;    "clean-reformatted-energy-data" procedure
;;; Postconditions:
;;;    The first list of consolidated-production-table will be identical to the first element of
;;;        production-table, but with the 11th, 12th, and 13th element (starting from
;;;        an index of 0) removed and the 11th element replaced with the value
;;;        "Hydrocarbon Production*". The rest of consolidated-production-table will be identical
;;;        to the equivalent elements of production-table but with the 11th being
;;;        replaced with the sum of the 11th, 12th, and 13th elements, and the 14th
;;;        element of production-table being the 12th element of consolidated-production-table.
(define consolidate-production-hydrocarbons
  (lambda (production-table)
    (let ([working-production-table (reverse (cons (list 1) (cdr production-table)))])
      (let kernel ([consolidated-production-table null]
                   [production-table-remaining (cdr working-production-table)]
                   [new-row null]
                   [current-row (reverse (car working-production-table))]
                   [current-column 0])
        (cond [(null? production-table-remaining)
               (cons (append (take (car production-table) 11)
                             (list "Hydrocarbon Production*" "Coal Production"))
                     consolidated-production-table)]
              [(null? current-row)
               (kernel (cons new-row consolidated-production-table)
                       (cdr production-table-remaining)
                       null
                       (reverse (car production-table-remaining))
                       0)]
              [(= 1 current-column)
               (kernel consolidated-production-table
                       production-table-remaining
                       (cons (+ (car current-row) (cadr current-row) (caddr current-row)) new-row)
                       (cdddr current-row)
                       2)]
              [else
               (kernel consolidated-production-table
                       production-table-remaining
                       (cons (car current-row) new-row)
                       (cdr current-row)
                       (increment current-column))])))))

;;; Procedure:
;;;    consolidate-consumption-hydrocarbons
;;; Parameters:
;;;    consumption-table, a table
;;; Purpose:
;;;    to combine the natural gas and petroleum statistics from a consumption table.
;;; Produces:
;;;    consolidated-consumption-table, a table
;;; Preconditions:
;;;    consumption-table must be a consumption table that has been put through the
;;;    "clean-reformatted-energy-data" procedure
;;; Postconditions:
;;;    The first list of consolidated-consumption-table will be identical to the first element of
;;;        consumption-table, but with the 11th and 12th element (starting from
;;;        an index of 0) removed and the 11th element replaced with the value
;;;        "Hydrocarbon Consumption (excluding Biomass Fuel)*". The rest of
;;;        consolidated-consumption-table will be identical to the equivalent elements of
;;;        consumption-table but with the 11th being replaced with the sum of the 11th and
;;;        12th elements, and the 13th element of consumption-table being the 12th element of
;;;        consolidated-consumption-table.
(define consolidate-consumption-hydrocarbons
  (lambda (production-table)
    (let ([working-consumption-table (reverse (cons (list 1) (cdr production-table)))])
      (let kernel ([consolidated-consumption-table null]
                   [consumption-table-remaining (cdr working-consumption-table)]
                   [new-row null]
                   [current-row (reverse (car working-consumption-table))]
                   [current-column 0])
        (cond [(null? consumption-table-remaining)
               (cons (append (take (car production-table) 11)
                             (list "Hydrocarbon Consumption (excluding Biomass Fuel)*" "Coal Consumption"))
                     consolidated-consumption-table)]
              [(null? current-row)
               (kernel (cons new-row consolidated-consumption-table)
                       (cdr consumption-table-remaining)
                       null
                       (reverse (car consumption-table-remaining))
                       0)]
              [(= 1 current-column)
               (kernel consolidated-consumption-table
                       consumption-table-remaining
                       (cons (+ (car current-row) (cadr current-row)) new-row)
                       (cddr current-row)
                       2)]
              [else
               (kernel consolidated-consumption-table
                       consumption-table-remaining
                       (cons (car current-row) new-row)
                       (cdr current-row)
                       (increment current-column))])))))


;=======================================================================================================================
;4;COMPARE
;==============================
;;; Procedure:
;;;   create-popularity-ratio-table
;;; Parameters:
;;;   consumption-table, a table
;;;   production-table, a table
;;; Purpose:
;;;   to generate popularity (consumption/production) data for analysis
;;; Produces:
;;;   popularity-ratio-table, a table
;;; Preconditions:
;;;   production-table must have been run through the consolidate-production-hydrocarbons
;;;       procedure.
;;;   consumption-table must have been run through the consolidate-consumption-hydrocarbons
;;;       procedure.
;;;   both production-table and consumption-table must have been run through the
;;;       clean-reformatted-energy-data procedure.
;;; Postconditions:
;;;   popularity-table will have as its first element, an identical header to production-table
;;;       but with the string " ratio" after all but the first two elements.
;;;   popularity-table will have all of the first and second elements of all lists in the table
;;;       identical to production-table.
;;;   popularity table will have, for all elements not previously specified, the result of division
;;;       with the equivalent value from consumption-table as the numerator and the equivalent value
;;;       from production-table as the denominator in place of the value.
(define create-popularity-ratio-table
  (lambda (consumption-table production-table)
    (let ([round-to-sig-figs (lambda (num figs)
                               (let ([fig-factor (reduce * (make-list figs 10))])
                                 (/ (round (* num fig-factor)) fig-factor)))]
          [working-consumption-table (reverse (cons (list 1) (cdr consumption-table)))]
          [working-production-table (reverse (cons (list 1) (cdr production-table)))]
          [new-header (list "Year" "Month" "Total Primary Energy Popularity Ratio"
                            "Total Renewable Energy Popularity Ratio" "Biomass Energy Popularity Ratio"
                            "Wind Energy Popularity Ratio" "Solar Energy Popularity Ratio"
                            "Geothermal Energy Popularity Ratio" "Hyrdroelectric Power Popularity Ratio"
                            "Nuclear Electric Power Popularity Ratio" "Total Fossil Fuels Popularity Ratio"
                            "Hydrocarbon Popularity Ratio*" "Coal Popularity Ratio")])
      (let kernel ([popularity-ratio-table null]
                   [consumption-table-remaining (cdr working-consumption-table)]
                   [production-table-remaining (cdr working-production-table)]
                   [consumption-working-row (reverse (car working-consumption-table))]
                   [production-working-row (reverse (car working-production-table))]
                   [current-column 0]
                   [new-row null])
        (cond [(and (null? consumption-table-remaining) (null? production-table-remaining))
               (cons new-header popularity-ratio-table)]
              [(= current-column 11)
               (kernel (cons (cons (cadr consumption-working-row)
                                   (cons (car consumption-working-row) new-row))
                             popularity-ratio-table)
                       (cdr consumption-table-remaining)
                       (cdr production-table-remaining)
                       (reverse (car consumption-table-remaining))
                       (reverse (car production-table-remaining))
                       0
                       null)]
              [else
               (kernel popularity-ratio-table
                       consumption-table-remaining
                       production-table-remaining
                       (cdr consumption-working-row)
                       (cdr production-working-row)
                       (increment current-column)
                       (cons (round-to-sig-figs (/ (car consumption-working-row)
                                                   (car production-working-row)) 6)
                             new-row))])))))




(define usable-production-data
  (consolidate-production-hydrocarbons (clean-reformatted-energy-data (reformat-energy-data energy-production-data))))
;  cleaned-reformatted-energy-production-data))

(define usable-consumption-data
  (consolidate-consumption-hydrocarbons (clean-reformatted-energy-data (reformat-energy-data energy-consumption-data))))
; cleaned-reformatted-energy-consumption-data)

(define popularity-ratio-table
  (create-popularity-ratio-table usable-consumption-data usable-production-data))

;=======================================================================================================================
;5;MAKE PLOT-ABLE 
;==============================

;;; Procedure:
;;;   make-time-scale-metric
;;; Parameters:
;;;   table, a table
;;; Purpose:
;;;    produce a 1D metric, which can be thoguht of as a number line, for consolidated,
;;;        cleaned, reformatted energy data.
;;; Produces:
;;;    time-scale-metric, a list of integers
(define make-time-scale-metric
  (lambda (table)
    (let ([working-table (reverse (cons (list 0 0) (cdr table)))])
      (let kernel ([time-scale-metric null]
                   [table-remaining (cdr working-table)]
                   [first-digit (cadar working-table)]
                   [second-digit (caar working-table)])
        (cond [(null? table-remaining)
               time-scale-metric]
              [else
               (kernel ;the line below converts our month and year into an integer that
                ;represents the number of months from January, 1984.
                (cons (+ first-digit (* 12 (- second-digit 1984))) time-scale-metric)
                (cdr table-remaining)
                (cadar table-remaining)
                (caar table-remaining))])))))

;;; Procedure:
;;;   make-logarithmic-data-points
;;; Parameters:
;;;   table, a table
;;; Purpose:
;;;   to generate logarithmically scaled plotting data for popularity table
;;; Produces:
;;;   logarithmic-data-table, a plottable data table
;;; Preconditions:
;;;    table must have at least three columns.
;;;    All non-header entries must be numbers.
;;;    All non-header entries not in the zeroth and first columns must be greater than 0.
;;; Postconditions:
;;;    For each non-header entry outside of the zeroth and first columns, make-logarithmic-data-points
;;;        replaces that entry with a data point, which is a list consisting of a number and the log of
;;;        each entry with resepect to 10. The numbers in each data point are provided by running the
;;;        make-time-scale-metric procedure on table. No other changes are made to the structure of the data.
(define make-logarithmic-data-points
  (lambda (table)
    (let* ([time-values (reverse (cons 0 (make-time-scale-metric table)))]
           [working-table (reverse (cons (make-list 11 0) (map cddr (cdr table))))]
           [data-headers (map (o (section list "Timestep" <>) (section string-replace <>  " Popularity Ratio" "")) (cddar table))])
      (let kernel ([logarithmic-data-table null]
                   [table-remaining (cdr working-table)]
                   [new-row null]
                   [current-row (reverse (car working-table))]
                   [current-time-value (car time-values)]
                   [time-values-remaining (cdr time-values)])
        (cond [(null? table-remaining)
               (cons data-headers logarithmic-data-table)]
              [(null? current-row)
               (kernel (cons new-row logarithmic-data-table)
                       (cdr table-remaining)
                       null
                       (reverse (car table-remaining))
                       (car time-values-remaining)
                       (cdr time-values-remaining))]
              [else
               (kernel logarithmic-data-table
                       table-remaining
                       (cons (cons current-time-value (cons (/ (log (car current-row)) (log 10)) null)) new-row)
                       (cdr current-row)
                       current-time-value
                       time-values-remaining)])))))

;;; Procedure:
;;;   make-linear-data-points
;;; Parameters:
;;  table, a table
;;; Purpose:
;;;   to generate linearly scaled plotting data for popularity table
;;; Produces:
;;;   linear-data-table, a plottable data table
;;; Preconditions:
;;;    table must have at least three columns.
;;;    All non-header entries must be numbers.
;;; Postconditions:
;;;    For each non-header entry outside of the zeroth and first columns, make-linear-data-points
;;;        replaces that entry with a data point, which is a list consisting of a number and each entry. T
;;;        The numbers in each data point are provided by running the make-time-scale-metric procedure
;;;        on table. No other changes are made to the structure of the data.
(define make-linear-data-points
  (lambda (table)
    (let* ([time-values (reverse (cons 0 (make-time-scale-metric table)))]
           [working-table (reverse (cons (make-list 11 0) (map cddr (cdr table))))]
           [data-headers (map (o (section list "Timestep" <>) (section string-replace <>  " Popularity Ratio" "")) (cddar table))])
      (let kernel ([linear-data-table null]
                   [table-remaining (cdr working-table)]
                   [new-row null]
                   [current-row (reverse (car working-table))]
                   [current-time-value (car time-values)]
                   [time-values-remaining (cdr time-values)])
        (cond [(null? table-remaining)
               (cons data-headers linear-data-table)]
              [(null? current-row)
               (kernel (cons new-row linear-data-table)
                       (cdr table-remaining)
                       null
                       (reverse (car table-remaining))
                       (car time-values-remaining)
                       (cdr time-values-remaining))]
              [else
               (kernel linear-data-table
                       table-remaining
                       (cons (cons current-time-value (cons (car current-row) null)) new-row)
                       (cdr current-row)
                       current-time-value
                       time-values-remaining)])))))



(define linear-popularity-ratio-data-points
  (make-linear-data-points popularity-ratio-table))

(define logarithmic-popularity-ratio-data-points
  (make-logarithmic-data-points popularity-ratio-table))


;;; Procedure:
;;;   get-data
;;; Parameters:
;;;   table, a plottable data table
;;;   num, an integer
;;; Purpose:
;;;    to extract all the data points from table.
;;; Produces:
;;;    data-points, a list of data points
;;; Preconditions:
;;;    table must have been run through make-data-points or a similar procedure.
;;;    0 < integer < # of columns in table - 2
;;; Postconditions:
;;;    (get-data table num) = (map (section list-ref <> num) table)
;;;     We know this is literally just the procedure but there is no better way to describe it.
;;;
(define get-data
  (lambda (table num)
    (map (section list-ref <> num) table)))

(define total-primary-energy-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 0))
(define total-renewable-energy-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 1))
(define total-renewable-energy-logarithmic-popularity-ratio-data
  (get-data logarithmic-popularity-ratio-data-points 1))
(define biomass-energy-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 2))
(define wind-energy-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 3))
(define solar-energy-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 4))
(define geothermal-energy-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 5))
(define hydroelectric-power-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 6))
(define nuclear-electric-power-popularity-ratio-data
  (get-data linear-popularity-ratio-data-points 7))
(define total-fossil-fuels-popularity-ratio-data
  (get-data logarithmic-popularity-ratio-data-points 8))
(define hydrocarbon-popularity-ratio-data
  (get-data logarithmic-popularity-ratio-data-points 9))
;(define combined-natural-gas-popularity-ratio-data
; (get-data linear-popularity-ratio-data-points-log 10))
(define coal-popularity-ratio-data
  (get-data logarithmic-popularity-ratio-data-points 10))


;=======================================================================================================================
;6;PLOT
;==============================

(define carbon-energy-plot
  (plot (list
         (function (lambda (x) 0) #:color "purple" #:style 'dot #:width 0.75)
         (lines (cdr total-fossil-fuels-popularity-ratio-data)  #:color "slategray" #:style 'dot-dash #:label (cadar total-fossil-fuels-popularity-ratio-data))
         (lines (cdr hydrocarbon-popularity-ratio-data)  #:color "black" #:style 'short-dash #:label (cadar hydrocarbon-popularity-ratio-data))
         ;(lines (cdr combined-natural-gas-popularity-ratio-data) #:color "blue" #:style 'long-dash  #:label (cadar combined-natural-gas-popularity-ratio-data))
         (lines (cdr coal-popularity-ratio-data) #:color "brown" #:style 'dot #:label (cadar coal-popularity-ratio-data)))
        #:title "Popularity Ratios over Time for Carbon Sources of Energy"
        #:x-label "Time (Months from January 1984 to August 2018)"
        #:y-label "Ratio (log base 10)"
        #:x-min 0
        #:x-max 420
        #:y-min -0.5
        #:y-max 1.0
        #:width 1000
        #:height 1000))

(define renewable-energy-plot
  (plot (list
         ;(function (lambda (x) 1) #:color "purple" #:style 'dot #:width 0.75)
         (lines (cdr total-renewable-energy-popularity-ratio-data)  #:color "purple" #:style 'dot-dash #:width 1.3 #:label (cadar total-renewable-energy-popularity-ratio-data))
         (lines (cdr biomass-energy-popularity-ratio-data)  #:color "darkgreen" #:style 'long-dash #:label (cadar biomass-energy-popularity-ratio-data))
         (lines (cdr wind-energy-popularity-ratio-data)  #:color "slategray" #:style 'dot #:width 1.3 #:label (cadar wind-energy-popularity-ratio-data))
         (lines (cdr solar-energy-popularity-ratio-data)  #:color "darkorange" #:style 'long-dash #:width 1.3 #:label (cadar solar-energy-popularity-ratio-data))
         (lines (cdr geothermal-energy-popularity-ratio-data)  #:color "brown" #:style 'short-dash #:width 1.3 #:label (cadar geothermal-energy-popularity-ratio-data))
         (lines (cdr hydroelectric-power-popularity-ratio-data)  #:color "darkblue" #:style 'dot #:width 1.3 #:label (cadar hydroelectric-power-popularity-ratio-data))
         (lines (cdr nuclear-electric-power-popularity-ratio-data)  #:color "red" #:style 'dot #:width 1.3 #:label (cadar nuclear-electric-power-popularity-ratio-data)))
        #:title "Popularity Ratios over Time for Non-Carbon Based Sources of Energy"
        #:x-label "Time (Months from January 1984 to August 2018)"
        #:y-label "Ratio"
        #:x-min 0
        #:x-max 420
        #:y-min 0.9
        #:y-max 1.05
        #:width 1000
        #:height 1000))

(define total-primary-energy-plot
  (plot (list
         (function (lambda (x) 0) #:color "purple" #:style 'dot #:width 0.75)
         (lines (cdr total-primary-energy-popularity-ratio-data) #:color 0 #:style 'solid #:label (cadar total-primary-energy-popularity-ratio-data)))
        #:title "Popularity Ratio over Time for Total Primary Energy"
        #:x-label "Time (Months from January 1984 to August 2018)"
        #:y-label "Ratio (log base 10)"
        #:x-min 0
        #:x-max 420
        #:y-min 0.7
        #:y-max 2.0
        #:width 1000
        #:height 1000))

(define compare-totals-plot
  (plot (list
         (lines (cdr total-fossil-fuels-popularity-ratio-data)  #:color "red" #:style 'dot-dash #:label (cadar total-fossil-fuels-popularity-ratio-data))
         (lines (cdr total-renewable-energy-logarithmic-popularity-ratio-data)  #:color "purple" #:style 'dot-dash #:width 1.3 #:label (cadar total-renewable-energy-popularity-ratio-data)))
        #:title "Total Fossil-Fuel Popularity vs Total Renewable Popularity"
        #:x-label "Time (Months from January 1984 to August 2018)"
        #:y-label "Ratio (log base 10)"
        #:x-min 0
        #:x-max 420
        #:y-min -0.2
        #:y-max 0.5
        #:width 1000
        #:height 1000))

;===================================================================================================================