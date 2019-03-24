(in-package :cl-user)
(defpackage calendar-date
  (:use :cl)
  (:export :calendar-date
           :calendar-date-today
           :calendar-date-year
           :calendar-date-month
           :calendar-date-day
           :calendar-date-values
           :calendar-date-day-of-week
           :calendar-date=
           :calendar-date/=
           :calendar-date<
           :calendar-date>
           :calendar-date<=
           :calendar-date>=
           :business-day-p
           :weekday-p
           :weekend-p
           :next-day
           :next-weekday
           :previous-day
           :next-week
           :previous-week
           :same-day-of-week-of-next-week
           :same-day-of-week-of-previous-week
           :day-of-week-of-the-week
           :next-month
           :previous-month
           :same-day-of-next-month
           :same-day-of-previous-month
           :first-of-the-month
           :nth-of-the-month
           :nth-of-the-month-in-business
           :nth-business-day-of-the-month
           :last-day-of-the-month
           :last-business-day-of-the-month)
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

(defun last-day-of-year-month (year month)
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
  (check-type year (integer 0 9999))
  (check-type month (integer 1 12))
  (check-type day (integer 1 31))
  (unless (<= day (last-day-of-year-month year month))
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
  (check-type year (integer 0 9999))
  (check-type month (integer 1 12))
  (check-type day (integer 1 31))
  (unless (<= day (last-day-of-year-month year month))
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

(defun calendar-date-day-of-week (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (day-of-week year month day)))

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

(defun business-day-p (calendar-date)
  (weekday-p calendar-date))

(defun weekday-p (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (<= 1 (day-of-week year month day) 5)))

(defun weekend-p (calendar-date)
  (not (weekday-p calendar-date)))

(defun next-day (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (incf day)
    (when (> day (last-day-of-year-month year month))
      (setf day 1)
      (incf month))
    (when (> month 12)
      (setf month 1)
      (incf year))
    (calendar-date year month day)))

(defun next-weekday (calendar-date)
  (loop
     do (setf calendar-date (next-day calendar-date))
     until (weekday-p calendar-date))
  calendar-date)

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
          (setf day (last-day-of-year-month year month))))
    (calendar-date year month day)))

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
        (let ((nth (min day (last-day-of-year-month year1 month1))))
          (nth-of-the-month nth calendar-date1))))))

(defun same-day-of-previous-month (calendar-date)
  (let ((day (calendar-date-day calendar-date)))
    (let ((calendar-date1 (previous-month calendar-date)))
      (multiple-value-bind (year1 month1 day1)
          (calendar-date-values calendar-date1)
        (declare (ignore day1))
        (let ((nth (min day (last-day-of-year-month year1 month1))))
          (nth-of-the-month nth calendar-date1))))))

(defun first-of-the-month (calendar-date)
  (nth-of-the-month 1 calendar-date))

(defun nth-of-the-month (nth calendar-date)
  (check-type nth (integer 1 31))
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (declare (ignore day))
    (unless (<= nth (last-day-of-year-month year month))
      (error "~A ~S does not have day ~S." (month-name month) year nth)))
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (declare (ignore day))
    (calendar-date year month nth)))

(defun nth-of-the-month-in-business (nth calendar-date)
  (let ((calendar-date1 (nth-of-the-month nth calendar-date)))
    (loop
       until (business-day-p calendar-date1)
       do (setf calendar-date1 (previous-day calendar-date1)))
    calendar-date1))

(defun nth-business-day-of-the-month (nth calendar-date)
  (check-type nth (integer 1))
  (let ((month (calendar-date-month calendar-date)))
    (let ((calendar-date1 (first-of-the-month calendar-date)))
      (loop
         do ;; Decrement counter if business day.
            (when (business-day-p calendar-date1)
              (decf nth))
            ;; Return the calendar date if counter reachs zero.
            (when (= nth 0)
              (return calendar-date1))
            ;; Proceeds the calendar date.
            (setf calendar-date1 (next-day calendar-date1))
            ;; Error if steps into the next month.
            (let ((month1 (calendar-date-month calendar-date1)))
              (unless (= month month1)
                (error "The value ~S is invalid." nth)))))))

(defun last-day-of-the-month (calendar-date)
  (multiple-value-bind (year month day) (calendar-date-values calendar-date)
    (declare (ignore day))
    (let ((day1 (last-day-of-year-month year month)))
      (calendar-date year month day1))))

(defun last-business-day-of-the-month (calendar-date)
  (let ((calendar-date1 (last-day-of-the-month calendar-date)))
    (loop until (business-day-p calendar-date1)
       do (setf calendar-date1 (previous-day calendar-date1)))
    calendar-date1))
