(require '[clojure.string :as str])

;;Gets the month out of the passed in date and converts it to an integer
(defn get-month [date-string]
  (let [resultsFromSplit (clojure.string/split date-string #"/")]
       (. Integer parseInt (nth resultsFromSplit 0))
       ))

;;Gets the day out of the passed in date and converts it to an integer     
(defn get-date [date-string]
    (let [resultsFromSplit (clojure.string/split date-string #"/")]
       (. Integer parseInt (nth resultsFromSplit 1))
       )
  )

;;Gets the year out of the passed in date and converts it to an integer
(defn get-year [date-string]
    (let [resultsFromSplit (clojure.string/split date-string #"/")]
       (. Integer parseInt (nth resultsFromSplit 2))
       )
  )

;;Returns true if the number is divisible by 4 or false if not
(defn is-divisible-by-4 [year]
  (= (mod year 4) 0)
  )

;;Returns true if the number is divisible by 100 or false if not
(defn is-divisible-by-100 [year]
  (= (mod year 100) 0)
  )

;;Returns true if the number is divisible by 400 or false if not
(defn is-divisible-by-400 [year]
  (= (mod year 400) 0)
  )

;;Returns true if the year is in fact a leap year, false if not
(defn isLeapYear [year]
    (cond 
      ;;Passes the century test
      (and (and (is-divisible-by-4 year) (is-divisible-by-100 year)) (is-divisible-by-400 year)) true
      ;;Fails the century test
      (and (and (is-divisible-by-4 year) (is-divisible-by-100 year)) (not (is-divisible-by-400 year))) false
      ;;Not a number divisible by 100 but is by 4 so it is a leap year
      (and (is-divisible-by-4 year) (not (is-divisible-by-100 year)) ) true
      ;;Cannot be a leap year if not divisible by 4
      (not (is-divisible-by-4 year)) false
      :else false))
    
;;Gets the number of days in a specific month 
(defn getDaysInMonth [month year]
  (cond
    (= month 1) 31
    ;;If it is a leap year, returns 29 days in February instead of 28
    (and (isLeapYear year) (= month 2)) 29
    (and (not (isLeapYear year)) (= month 2)) 28
    (= month 3) 31
    (= month 4) 30
    (= month 5) 31
    (= month 6) 30
    (= month 7) 31
    (= month 8) 31
    (= month 9) 30
    (= month 10) 31
    (= month 11) 30
    (= month 12) 31
    ))
  
;;Returns 365 if not a leap year and 366 if it is a leap year
(defn getDaysInYear [year]
  (cond
    (isLeapYear year) 366
    :else 365
    )
  )

;;Returns true if the start year and end year are the same, false if not
(defn sameYear [startYear endYear]
  (= startYear endYear)
)
  
;;Gets the days left in the month by obtaining the days in the month and subtracting it by how many days have passed
(defn getDaysLeftInMonth [month day year]
  (-(getDaysInMonth month year) day ))

;;Adds all the days in months that have passed up to the current month
(defn getDaysInMonthsFromStartOfYear [month year]
  (reduce + (for[x (range 1 month)]
        (getDaysInMonth x year))))

;;Adds all the days in years up to the current year
(defn getDaysInBetweenYears [startYear endYear]
  (reduce + (for [x (range startYear endYear)]
                (getDaysInYear x))))
              
;;Gets the days left in the year from the passed in month
(defn getDaysInMonthsUntilEndOfYear [month year]
  (reduce + (for[x (range month 13)]
          (getDaysInMonth x year))))
        
;;Called to calculate the total days that have passed between two dates if the two dates have the same year       
(defn daysFromSameYear [startDay startMonth startYear endDay endMonth endYear]
  ;;Gets the days from the being of the year for the start date and then the end date and subtracts the two totals
  (let [startDayNum ( + (getDaysInMonthsFromStartOfYear startMonth startYear) startDay) endDayNum ( + (getDaysInMonthsFromStartOfYear endMonth endYear) endDay)]
       (- endDayNum startDayNum)
       ))

;;Called to calculate the total days that have passed between two dates if the two date have different years     
(defn daysFromDifferentYears [startDay startMonth startYear endDay endMonth endYear]
  ;;Gets individual totals of the days left in the month from the start date, days until the end of the year from the start date, then the days in between years from the start to end date, and finally finds the days in months from the start of the year for the end date and adds all these totals to the end date to find overall total days passed
  (let [DaysLeftInMonthTotal (getDaysLeftInMonth startMonth startDay startYear) DaysInMonthsUntilEndOfYearTotal (getDaysInMonthsUntilEndOfYear (+ startMonth 1) startYear)
    DaysInBetweenYearsTotal (getDaysInBetweenYears (+ startYear 1) endYear)
    DaysInMonthsFromStartOfYearTotal (getDaysInMonthsFromStartOfYear endMonth endYear)]
  (+ DaysLeftInMonthTotal DaysInMonthsUntilEndOfYearTotal DaysInBetweenYearsTotal DaysInMonthsFromStartOfYearTotal endDay)
  )
  )

;;Has two implementations the first is when there are no parameters passed the second is when two dates are provided by the user, essentially the function calls eiter same year calc or different year calc to get the total number of days passed between two dates       
(defn get-number-of-days-in-between
  ;;Promts the user for a start date and end date which is then passed to this function again with parameters
  ([] (println "Enter in the first date?")
      (def startDate (read-line))
      (println "Enter in the second date?")
      (def endDate (read-line))
      (println (get-number-of-days-in-between startDate endDate)))
    ;;Second implementation
  ([startDate endDate]
  ;;Creates local variables that hold startDay startMonth startYear endDay endMonth endYear based on the user input 
  (let [startDay (get-date startDate) startMonth (get-month startDate) startYear (get-year startDate) endDay (get-date endDate) endMonth (get-month endDate) endYear (get-year endDate)]
       (println "Days Between" startDate "and" endDate)
       ;;If the user puts inputs two dates with the same year, calls daysFromSameYear, if the dates have different years, calls daysFromDifferentYears
       (cond
         (sameYear startYear endYear) (daysFromSameYear startDay startMonth startYear endDay endMonth endYear)
         :else (daysFromDifferentYears startDay startMonth startYear endDay endMonth endYear)
         )))
)

;;Sample outputs
(println (get-number-of-days-in-between "12/2/2017" "12/7/2017"))
(println (get-number-of-days-in-between "3/19/1973" "11/3/2017"))
(println (get-number-of-days-in-between "1/1/2000" "11/3/2017"))
(get-number-of-days-in-between)
