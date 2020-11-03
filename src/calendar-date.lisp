(in-package :cl-user)
(defpackage calendar-date
  (:use :cl)
  (:export :calendar-date
           :calendar-date-today
           :calendar-date-year
           :calendar-date-month
           :calendar-date-day
           :calendar-date-values
           :calendar-date-in-week
           :calendar-date-day-of-week
           :calendar-date-values-in-week
           :calendar-date=
           :calendar-date/=
           :calendar-date<
           :calendar-date>
           :calendar-date<=
           :calendar-date>=
           :weekday-p
           :weekend-p
           :next-day
           :previous-day
           :next-weekday
           :previous-weekday
           :next-week
           :previous-week
           :same-day-of-week-of-next-week
           :same-day-of-week-of-previous-week
           :next-day-of-week
           :previous-day-of-week
           :day-of-week-of-the-week
           :nth-day-of-week-of-the-month
           :next-month
           :previous-month
           :same-day-of-next-month
           :same-day-of-previous-month
           :first-of-the-month
           :nth-of-the-month
           :nth-weekday-of-the-month
           :last-day-of-the-month
           :last-weekday-of-the-month
           :nth-last-day-of-the-month
           :nth-last-weekday-of-the-month)
  (:import-from :local-time
                :now
                :timestamp-year
                :timestamp-month
                :timestamp-day
                :*default-timezone*))
(in-package :calendar-date)


(defun leap-year-p (year)
  (and (= (mod year 4) 0)
       (or (/= (mod year 100) 0)
           (= (mod year 400) 0))))

(defun %days-of-month (year month)
  (ecase month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))))

(defun month-name (month)
  (check-type month (integer 1 12))
  (nth (1- month) '("Jan." "Feb." "Mar." "Apr." "May." "Jun."
                    "Jul." "Aug." "Sep." "Oct." "Nov." "Dec.")))

(defun day-of-week (year month day)
  "Compute day of week in Gregorian calendar by Zeller's congruence."
  (check-type year (integer 1 9999))
  (check-type month (integer 1 12))
  (check-type day (integer 1 31))
  (unless (<= day (%days-of-month year month))
    (error "~A ~S does not have day ~S." (month-name month) year day))
  (flet ((div (x y) (floor (/ x y))))
    (let ((year1 (if (< month 3)
                     (1- year)
                     year))
          (month1 (if (< month 3)
                      (+ month 12)
                      month)))
      (let* ((c (div year1 100))
             (y (mod year1 100))
             (g (+ (* 5 c) (div c 4))))
        (1+ (mod (+ day (div (* (1+ month1) 26) 10) y (div y 4) g 5) 7))))))

(defstruct (calendar-date (:constructor %make-calendar-date))
  (year :year :type integer :read-only t)
  (month :month :type integer :read-only t)
  (day :day :type integer :read-only t))

(defun calendar-date (year month day)
  (check-type year (integer 1 9999))
  (check-type month (integer 1 12))
  (check-type day (integer 1 31))
  (unless (<= day (%days-of-month year month))
    (error "~A ~S does not have day ~S." (month-name month) year day))
  (%make-calendar-date :year year :month month :day day))

(defun calendar-date-today (&key (timezone *default-timezone*))
  (let ((now (now)))
    (calendar-date (timestamp-year now :timezone timezone)
                   (timestamp-month now :timezone timezone)
                   (timestamp-day now :timezone timezone))))

(defun calendar-date-values (calendar-date)
  (values (calendar-date-year calendar-date)
          (calendar-date-month calendar-date)
          (calendar-date-day calendar-date)))

(defun %weeks-per-year (year)
  ;; ref. https://en.wikipedia.org/wiki/ISO_week_date#Last_week
  (flet ((p (y)
           (mod (+ y (floor y 4) (- (floor y 100)) (floor y 400))
                7)))
    (if (or (= 4 (p year))
            (= 3 (p (1- year))))
        53
        52)))

(defun %long-year-p (year)
  (= (%weeks-per-year year) 53))

(defun %days-of-year (year)
  (if (leap-year-p year)
      366
      365))

(defun %ordinal-date-from-week-date (year woy dow)
  ;; https://en.wikipedia.org/wiki/ISO_week_date#Calculating_an_ordinal_or_month_date_from_a_week_date
  (let ((d (- (+ (* woy 7) dow)
              (day-of-week year 1 4)
              3)))
    (cond
      ((< d 1) (let* ((year1 (1- year))
                      (doy (+ d (%days-of-year year1))))
                 (values year1 doy)))
      ((> d (%days-of-year year)) (let ((doy (- d (%days-of-year year))))
                                    (values (1+ year) doy)))
      (t (values year d)))))

(defparameter +days-of-month+
  '(31 28 31 30 31 30 31 31 30 31 30 31))

(defun %date-from-doy (year doy)
  (loop with month = 1
     for days = (%days-of-month year month)
     when (<= doy days)
     return (values month doy)
     do (incf month)
        (decf doy days)))

(defun calendar-date-in-week (year woy dow)
  (check-type year (integer 1 9999))
  (check-type woy (integer 1 53))
  (check-type dow (integer 1 7))
  (when (= woy 53)
    (unless (%long-year-p year)
      (error "~A is a short year." year)))
  (multiple-value-bind (year1 doy) (%ordinal-date-from-week-date year woy dow)
    (multiple-value-bind (month day) (%date-from-doy year1 doy)
      (%make-calendar-date :year year1 :month month :day day))))

(defun calendar-date-day-of-week (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (day-of-week year month day)))

(defparameter +offset-ordinal-date+
  '(0 31 59 90 120 151 181 212 243 273 304 334))

(defparameter +offset-ordinal-date-leap+
  '(0 31 60 91 121 152 182 213 244 274 305 335))

(defun %ordinal-date-from-month-date (year month day)
  ;; https://en.wikipedia.org/wiki/ISO_week_date#Calculating_the_week_number_from_a_month_and_day_of_the_month_or_ordinal_date
  (let ((offset (if (leap-year-p year)
                    +offset-ordinal-date-leap+
                    +offset-ordinal-date+)))
    (+ (nth (1- month) offset) day)))

(defun calendar-date-values-in-week (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (let* ((doy (%ordinal-date-from-month-date year month day))
           (dow (day-of-week year month day))
           (w (floor (+ (- doy dow) 10) 7)))
      (cond
        ((< w 1) (let* ((year1 (1- year))
                        (woy (%weeks-per-year year1)))
                   (values year1 woy dow)))
        ((> w (%weeks-per-year year)) (values (1+ year) 1 dow))
        (t (values year w dow))))))

(defun calendar-date= (calendar-date1 calendar-date2)
  (multiple-value-bind (year1 month1 day1)
      (calendar-date-values calendar-date1)
    (multiple-value-bind (year2 month2 day2)
        (calendar-date-values calendar-date2)
      (and (= year1 year2)
           (= month1 month2)
           (= day1 day2)))))

(defun calendar-date/= (calendar-date1 calendar-date2)
  (not (calendar-date= calendar-date1 calendar-date2)))

(defun calendar-date< (calendar-date1 calendar-date2)
  (multiple-value-bind (year1 month1 day1)
      (calendar-date-values calendar-date1)
    (multiple-value-bind (year2 month2 day2)
        (calendar-date-values calendar-date2)
      (or (< year1 year2)
          (and (= year1 year2)
               (< month1 month2))
          (and (= year1 year2)
               (= month1 month2)
               (< day1 day2))))))

(defun calendar-date> (calendar-date1 calendar-date2)
  (and (not (calendar-date< calendar-date1 calendar-date2))
       (not (calendar-date= calendar-date1 calendar-date2))))

(defun calendar-date<= (calendar-date1 calendar-date2)
  (not (calendar-date> calendar-date1 calendar-date2)))

(defun calendar-date>= (calendar-date1 calendar-date2)
  (not (calendar-date< calendar-date1 calendar-date2)))

(defun weekday-p (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (<= 1 (day-of-week year month day) 5)))

(defun weekend-p (calendar-date)
  (not (weekday-p calendar-date)))

(defun next-day (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (incf day)
    (when (> day (%days-of-month year month))
      (setf day 1)
      (incf month))
    (when (> month 12)
      (setf month 1)
      (incf year))
    (calendar-date year month day)))

(defun previous-day (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (decf day)
    (when (< day 1)
      (decf month)
      (if (< month 1)
          (progn
            (decf year)
            (setf month 12)
            (setf day 31))
          (setf day (%days-of-month year month))))
    (calendar-date year month day)))

(defun next-weekday (calendar-date)
  (loop
     do (setf calendar-date (next-day calendar-date))
     until (weekday-p calendar-date))
  calendar-date)

(defun previous-weekday (calendar-date)
  (loop
     do (setf calendar-date (previous-day calendar-date))
     until (weekday-p calendar-date))
  calendar-date)

(defun next-week (calendar-date)
  (day-of-week-of-the-week 1
   (same-day-of-week-of-next-week calendar-date)))

(defun previous-week (calendar-date)
  (day-of-week-of-the-week 1
   (same-day-of-week-of-previous-week calendar-date)))

(defun same-day-of-week-of-next-week (calendar-date)
  (loop repeat 7
     do (setf calendar-date (next-day calendar-date)))
  calendar-date)

(defun same-day-of-week-of-previous-week (calendar-date)
  (loop repeat 7
     do (setf calendar-date (previous-day calendar-date)))
  calendar-date)

(defun next-day-of-week (day-of-week calendar-date)
  (check-type day-of-week (integer 1 7))
  (loop do (setf calendar-date (next-day calendar-date))
	until (= day-of-week (calendar-date-day-of-week calendar-date)))
  calendar-date)

(defun previous-day-of-week (day-of-week calendar-date)
  (check-type day-of-week (integer 1 7))
  (loop do (setf calendar-date (previous-day calendar-date))
        until (= day-of-week (calendar-date-day-of-week calendar-date)))
  calendar-date)

(defun day-of-week-of-the-week (day-of-week calendar-date)
  (check-type day-of-week (integer 1 7))
  (let* ((day-of-week1 (calendar-date-day-of-week calendar-date))
         (delta (- day-of-week1 day-of-week)))
    (if (<= 0 delta)
        (loop repeat delta
           do (setf calendar-date (previous-day calendar-date)))
        (loop repeat (- delta)
           do (setf calendar-date (next-day calendar-date))))
    calendar-date))

(defun nth-day-of-week-of-the-month (nth day-of-week calendar-date)
  (check-type nth (integer 1 5))
  (check-type day-of-week (integer 1 7))
  (let ((calendar-date1 (next-day-of-week day-of-week
                         (previous-day
                          (first-of-the-month calendar-date)))))
    (loop repeat (1- nth)
       do (setf calendar-date1 (same-day-of-week-of-next-week calendar-date1)))
    (unless (= (calendar-date-month calendar-date)
               (calendar-date-month calendar-date1))
      (error "The value of NTH is invalid."))
    calendar-date1))

(defun next-month (calendar-date)
  (next-day
   (last-day-of-the-month calendar-date)))

(defun previous-month (calendar-date)
  (first-of-the-month
   (previous-day
    (first-of-the-month calendar-date))))

(defun same-day-of-next-month (calendar-date)
  (let ((day (calendar-date-day calendar-date)))
    (let ((calendar-date1 (next-month calendar-date)))
      (multiple-value-bind (year1 month1 day1)
          (calendar-date-values calendar-date1)
        (declare (ignore day1))
        (let ((nth (min day (%days-of-month year1 month1))))
          (nth-of-the-month nth calendar-date1))))))

(defun same-day-of-previous-month (calendar-date)
  (let ((day (calendar-date-day calendar-date)))
    (let ((calendar-date1 (previous-month calendar-date)))
      (multiple-value-bind (year1 month1 day1)
          (calendar-date-values calendar-date1)
        (declare (ignore day1))
        (let ((nth (min day (%days-of-month year1 month1))))
          (nth-of-the-month nth calendar-date1))))))

(defun first-of-the-month (calendar-date)
  (nth-of-the-month 1 calendar-date))

(defun nth-of-the-month (nth calendar-date)
  (check-type nth (integer 1 31))
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (declare (ignore day))
    (unless (<= nth (%days-of-month year month))
      (error "~A ~S does not have day ~S." (month-name month) year nth)))
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (declare (ignore day))
    (calendar-date year month nth)))

(defun nth-weekday-of-the-month (nth calendar-date)
  (check-type nth (integer 1))
  (let ((calendar-date1 (first-of-the-month calendar-date)))
    ;; Proceeds the calendar date.
    (loop repeat (1- nth)
       do (setf calendar-date1 (next-weekday calendar-date1)))
    ;; Error if steps into the next month.
    (let ((month (calendar-date-month calendar-date))
          (month1 (calendar-date-month calendar-date1)))
      (unless (= month month1)
        (error "The value ~S is invalid." nth)))
    calendar-date1))

(defun last-day-of-the-month (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (declare (ignore day))
    (let ((day1 (%days-of-month year month)))
      (calendar-date year month day1))))

(defun last-weekday-of-the-month (calendar-date)
  (let ((calendar-date1 (last-day-of-the-month calendar-date)))
    (loop until (weekday-p calendar-date1)
       do (setf calendar-date1 (previous-day calendar-date1)))
    calendar-date1))

(defun nth-last-day-of-the-month (nth calendar-date)
  (check-type nth (integer 1 31))
  (let ((calendar-date1 (last-day-of-the-month calendar-date)))
    ;; Regresses the calendar date.
    (loop repeat (1- nth)
       do (setf calendar-date1 (previous-day calendar-date1)))
    ;; Error if steps into the previous month.
    (let ((month (calendar-date-month calendar-date))
          (month1 (calendar-date-month calendar-date1)))
      (unless (= month month1)
        (error "The value ~S is invalid." nth)))
    calendar-date1))

(defun nth-last-weekday-of-the-month (nth calendar-date)
  (check-type nth (integer 1))
  (let ((calendar-date1 (last-weekday-of-the-month calendar-date)))
    ;; Regresses the calendar date.
    (loop repeat (1- nth)
       do (setf calendar-date1 (previous-weekday calendar-date1)))
    ;; Error if steps into the previous month.
    (let ((month (calendar-date-month calendar-date))
          (month1 (calendar-date-month calendar-date1)))
      (unless (= month month1)
        (error "The value ~S is invalid." nth)))
    calendar-date1))
