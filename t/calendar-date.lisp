(in-package :cl-user)
(defpackage calendar-date-test
  (:use :cl
        :calendar-date
        :prove)
  (:import-from :local-time
                :today
                :timestamp-year
                :timestamp-month
                :timestamp-day
                :*default-timezone*
                :+utc-zone+))
(in-package :calendar-date-test)


(plan nil)

(subtest "leap-year-p"

  (is (calendar-date::leap-year-p 2015)
      nil)

  (is (calendar-date::leap-year-p 2016)
      t)

  (is (calendar-date::leap-year-p 1900)
      nil)

  (is (calendar-date::leap-year-p 2000)
      t))

(subtest "%days-of-month"

  (is (calendar-date::%days-of-month 2015 1)
      31)

  (is (calendar-date::%days-of-month 2015 4)
      30)

  (is (calendar-date::%days-of-month 2015 2)
      28)

  (is (calendar-date::%days-of-month 2016 2)
      29))

(subtest "month-name"

  (is (calendar-date::month-name 1)
      "Jan."
      :test #'string=)

  (is (calendar-date::month-name 12)
      "Dec."
      :test #'string=)

  (is-error (calendar-date::month-name 0)
            type-error
            "invalid month.")

  (is-error (calendar-date::month-name :foo)
            type-error
            "invalid month."))

(subtest "day-of-week"

  ;; Day in ordinal year.
  (is (calendar-date::day-of-week 2015 1 1)
      4)

  ;; Day before March in leap year.
  (is (calendar-date::day-of-week 2016 1 1)
      5)

  ;; Day on March or after in leap year.
  (is (calendar-date::day-of-week 2016 3 1)
      2)

  (is-error (calendar-date::day-of-week -1 1 1)
            type-error
            "invalid year.")

  (is-error (calendar-date::day-of-week 10000 1 1)
            type-error
            "invalid year.")

  (is-error (calendar-date::day-of-week 2015 0 1)
            type-error
            "invalid month.")

  (is-error (calendar-date::day-of-week 2015 13 1)
            type-error
            "invalid month.")

  (is-error (calendar-date::day-of-week 2015 1 0)
            type-error
            "invalid day.")

  (is-error (calendar-date::day-of-week 2015 1 32)
            type-error
            "invalid day.")

  (is-error (calendar-date::day-of-week 2015 2 29)
            simple-error
            "invalid day."))

(subtest "calendar-date"

  (let ((calendar-date (calendar-date 2015 1 1)))
    (is (calendar-date-year calendar-date)
        2015)
    (is (calendar-date-month calendar-date)
        1)
    (is (calendar-date-day calendar-date)
        1))

  (is-error (calendar-date :foo 1 1)
            type-error
            "invalid year.")

  (is-error (calendar-date -1 1 1)
            type-error
            "invalid year.")

  (is-error (calendar-date 10000 1 1)
            type-error
            "invalid year.")

  (is-error (calendar-date 2015 :foo 1)
            type-error
            "invalid month.")

  (is-error (calendar-date 2015 0 1)
            type-error
            "invalid month.")

  (is-error (calendar-date 2015 13 1)
            type-error
            "invalid month.")

  (is-error (calendar-date 2015 1 0)
            type-error
            "invalid day.")

  (is-error (calendar-date 2015 1 32)
            type-error
            "invalid day.")

  (is-error (calendar-date 2015 2 29)
            simple-error
            "invalid day."))

(subtest "%weeks-per-year"
  (is (calendar-date::%weeks-per-year 2019)
      52)

  (is (calendar-date::%weeks-per-year 2020)
      53)
  )

(subtest "%long-year-p"
  (is (calendar-date::%long-year-p 2019)
      nil)

  (is (calendar-date::%long-year-p 2020)
      t)
  )

(subtest "calendar-date-in-week"

  (let ((calendar-date (calendar-date-in-week 2020 1 3)))
    (is (calendar-date-year calendar-date)
        2020)
    (is (calendar-date-month calendar-date)
        1)
    (is (calendar-date-day calendar-date)
        1))

  (let ((calendar-date (calendar-date-in-week 2020 1 1)))
    (is (calendar-date-year calendar-date)
        2019)
    (is (calendar-date-month calendar-date)
        12)
    (is (calendar-date-day calendar-date)
        30))

  (let ((calendar-date (calendar-date-in-week 2019 52 1)))
    (is (calendar-date-year calendar-date)
        2019)
    (is (calendar-date-month calendar-date)
        12)
    (is (calendar-date-day calendar-date)
        23))

  (let ((calendar-date (calendar-date-in-week 2020 53 7)))
    (is (calendar-date-year calendar-date)
        2021)
    (is (calendar-date-month calendar-date)
        1)
    (is (calendar-date-day calendar-date)
        3))

  (is-error (calendar-date-in-week 2020 0 1)
            type-error
            "invalid week")

  (is-error (calendar-date-in-week 2019 53 1)
            simple-error
            "invalid week")

  (is-error (calendar-date-in-week 2020 54 1)
            type-error
            "invalid week")

  (is-error (calendar-date-in-week 2020 1 0)
            type-error
            "invalid day of week")

  (is-error (calendar-date-in-week 2020 1 8)
            type-error
            "invalid day of week")
  )

(subtest "calendar-date-today"

  (let ((*default-timezone* +utc-zone+))
    (let ((today (today))               ; TODAY always in UTC.
          (calendar-date-today (calendar-date-today)))
      (is (calendar-date-year calendar-date-today)
          (timestamp-year today))
      (is (calendar-date-month calendar-date-today)
          (timestamp-month today))
      (is (calendar-date-day calendar-date-today)
          (timestamp-day today))))

  (is-error (calendar-date-today :timezone :foo)
            type-error
            "invalid timezone."))

(subtest "calendar-date-values"

  (is-values (calendar-date-values (calendar-date 2015 1 1))
             '(2015 1 1))

  (is-error (calendar-date-values :foo)
            type-error
            "invalid calendar date."))

(subtest "calendar-date-values-in-week"
  (is-values (calendar-date-values-in-week (calendar-date 2020 1 1))
             '(2020 1 3))

  (is-values (calendar-date-values-in-week (calendar-date 2019 12 30))
             '(2020 1 1))

  (is-values (calendar-date-values-in-week (calendar-date 2019 12 23))
             '(2019 52 1))

  (is-values (calendar-date-values-in-week (calendar-date 2021 1 3))
             '(2020 53 7))

  (is-error (calendar-date-values-in-week :foo)
            type-error
            "invalid calendar date.")
  )

(subtest "calendar-date-day-of-week"

  (is (calendar-date-day-of-week (calendar-date 2015 1 1))
      4)

  (is-error (calendar-date-day-of-week :foo)
            type-error
            "invalid calendar date."))

(subtest "calendar-date="

  (is (calendar-date= (calendar-date 2015 1 1)
                      (calendar-date 2015 1 1))
      t)

  (is (calendar-date= (calendar-date 2015 1 1)
                      (calendar-date 2015 1 2))
      nil)

  (is (calendar-date= (calendar-date 2015 1 1)
                      (calendar-date 2015 2 1))
      nil)

  (is (calendar-date= (calendar-date 2015 1 1)
                      (calendar-date 2016 1 1))
      nil)

  (is-error (calendar-date= :foo (calendar-date 2015 1 1))
            type-error
            "invalid calendar date.")

  (is-error (calendar-date= (calendar-date 2015 1 1) :foo)
            type-error
            "invalid calendar date."))

(subtest "calendar-date/="

  (is (calendar-date/= (calendar-date 2015 1 1)
                       (calendar-date 2015 1 1))
      nil)

  (is (calendar-date/= (calendar-date 2015 1 1)
                       (calendar-date 2015 1 2))
      t))

(subtest "calendar-date<"

  (is (calendar-date< (calendar-date 2015 1 1)
                      (calendar-date 2015 1 2))
      t)

  (is (calendar-date< (calendar-date 2015 1 1)
                      (calendar-date 2015 2 1))
      t)

  (is (calendar-date< (calendar-date 2015 1 1)
                      (calendar-date 2016 1 1))
      t)

  (is (calendar-date< (calendar-date 2014 12 31)
                      (calendar-date 2015 1 1))
      t)

  (is (calendar-date< (calendar-date 2015 1 1)
                      (calendar-date 2015 1 1))
      nil)

  (is (calendar-date< (calendar-date 2015 1 2)
                      (calendar-date 2015 1 1))
      nil)

  (is (calendar-date< (calendar-date 2015 2 1)
                      (calendar-date 2015 1 1))
      nil)

  (is (calendar-date< (calendar-date 2016 1 1)
                      (calendar-date 2015 1 1))
      nil))

(subtest "calendar-date>"

  (is (calendar-date> (calendar-date 2015 1 2)
                      (calendar-date 2015 1 1))
      t))

(subtest "calendar-date<="

  (is (calendar-date<= (calendar-date 2015 1 1)
                       (calendar-date 2015 1 1))
      t)

  (is (calendar-date<= (calendar-date 2015 1 1)
                       (calendar-date 2015 1 2))
      t))

(subtest "calendar-date>="

  (is (calendar-date>= (calendar-date 2015 1 1)
                       (calendar-date 2015 1 1))
      t)

  (is (calendar-date>= (calendar-date 2015 1 2)
                       (calendar-date 2015 1 1))
      t))

(subtest "weekday-p"

  (is (weekday-p (calendar-date 2015 1 1))
      t)

  (is (weekday-p (calendar-date 2015 1 3))
      nil)

  (is (weekday-p (calendar-date 2015 1 4))
      nil)

  (is-error (weekday-p :foo)
            type-error
            "invalid calendar date."))

(subtest "weekend-p"

  (is (weekend-p (calendar-date 2015 1 1))
      nil)

  (is (weekend-p (calendar-date 2015 1 3))
      t)

  (is (weekend-p (calendar-date 2015 1 4))
      t)

  (is-error (weekend-p :foo)
            type-error
            "invalid calendar date."))

(subtest "next-day"

  (is (next-day (calendar-date 2015 1 1))
      (calendar-date 2015 1 2)
      :test #'calendar-date=)

  (is (next-day (calendar-date 2015 1 31))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is (next-day (calendar-date 2015 12 31))
      (calendar-date 2016 1 1)
      :test #'calendar-date=)

  (is (next-day (calendar-date 2015 2 28))
      (calendar-date 2015 3 1)
      :test #'calendar-date=)

  (is (next-day (calendar-date 2016 2 28))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (next-day :foo)
            type-error
            "invalid calendar date."))

(subtest "previous-day"

  (is (previous-day (calendar-date 2015 1 2))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (previous-day (calendar-date 2015 2 1))
      (calendar-date 2015 1 31)
      :test #'calendar-date=)

  (is (previous-day (calendar-date 2016 1 1))
      (calendar-date 2015 12 31)
      :test #'calendar-date=)

  (is (previous-day (calendar-date 2015 3 1))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (previous-day (calendar-date 2016 3 1))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (previous-day :foo)
            type-error
            "invalid calendar date."))

(subtest "next-weekday"

  (is (next-weekday (calendar-date 2019 3 22))
      (calendar-date 2019 3 25)
      :test #'calendar-date=)

  (is (next-weekday (calendar-date 2019 3 25))
      (calendar-date 2019 3 26)
      :test #'calendar-date=)

  (is-error (next-weekday :foo)
            type-error
            "invalid calendar date."))

(subtest "previous-weekday"

  (is (previous-weekday (calendar-date 2019 3 25))
      (calendar-date 2019 3 22)
      :test #'calendar-date=)

  (is (previous-weekday (calendar-date 2019 3 22))
      (calendar-date 2019 3 21)
      :test #'calendar-date=)

  (is-error (previous-weekday :foo)
            type-error
            "invalid calendar date."))

(subtest "next-week"

  (is (next-week (calendar-date 2015 1 1))
      (calendar-date 2015 1 5)
      :test #'calendar-date=)

  (is-error (next-week :foo)
            type-error
            "invalid calendar date."))

(subtest "previous-week"

  (is (previous-week (calendar-date 2015 1 1))
      (calendar-date 2014 12 22)
      :test #'calendar-date=)

  (is-error (previous-week :foo)
            type-error
            "invalid calendar date."))

(subtest "same-day-of-week-of-next-week"

  (is (same-day-of-week-of-next-week (calendar-date 2015 1 1))
      (calendar-date 2015 1 8)
      :test #'calendar-date=)

  (is-error (same-day-of-week-of-next-week :foo)
            type-error
            "invalid calendar date."))

(subtest "same-day-of-week-of-previous-week"

  (is (same-day-of-week-of-previous-week (calendar-date 2015 1 1))
      (calendar-date 2014 12 25)
      :test #'calendar-date=)

  (is-error (same-day-of-week-of-previous-week :foo)
            type-error
            "invalid calendar date."))

(subtest "next-day-of-week"

  (is (next-day-of-week 1 (calendar-date 2019 3 28))
      (calendar-date 2019 4 1)
      :test #'calendar-date=)

  (is (next-day-of-week 4 (calendar-date 2019 3 28))
      (calendar-date 2019 4 4)
      :test #'calendar-date=)

  (is-error (next-day-of-week 0 (calendar-date 2019 3 28))
	    type-error
	    "invalid day of week.")

  (is-error (next-day-of-week 8 (calendar-date 2019 3 28))
	    type-error
	    "invalid day of week.")

  (is-error (next-day-of-week 1 :foo)
	    type-error
	    "invalid calendar date."))

(subtest "previous-day-of-week"

  (is (previous-day-of-week 1 (calendar-date 2019 3 28))
      (calendar-date 2019 3 25)
      :test #'calendar-date=)

  (is (previous-day-of-week 4 (calendar-date 2019 3 28))
      (calendar-date 2019 3 21)
      :test #'calendar-date=)

  (is-error (previous-day-of-week 0 (calendar-date 2019 3 28))
	    type-error
	    "invalid day of week.")

  (is-error (previous-day-of-week 8 (calendar-date 2019 3 28))
	    type-error
	    "invalid day of week.")

  (is-error (previous-day-of-week 1 :foo)
	    type-error
	    "invalid calendar date."))

(subtest "day-of-week-of-the-week"

  (is (day-of-week-of-the-week 1 (calendar-date 2015 1 1))
      (calendar-date 2014 12 29)
      :test #'calendar-date=)

  (is (day-of-week-of-the-week 7 (calendar-date 2015 1 1))
      (calendar-date 2015 1 4)
      :test #'calendar-date=)

  (is-error (day-of-week-of-the-week 0 (calendar-date 2015 1 1))
            type-error
            "invalid day of week.")

  (is-error (day-of-week-of-the-week 8 (calendar-date 2015 1 1))
            type-error
            "invalid day of week.")

  (is-error (day-of-week-of-the-week 1 :foo)
            type-error
            "invalid calendar date."))

(subtest "nth-day-of-week-of-the-month"

  (is (nth-day-of-week-of-the-month 1 1 (calendar-date 2019 3 1))
      (calendar-date 2019 3 4)
      :test #'calendar-date=)

  (is (nth-day-of-week-of-the-month 1 2 (calendar-date 2019 3 1))
      (calendar-date 2019 3 5)
      :test #'calendar-date=)

  (is (nth-day-of-week-of-the-month 4 1 (calendar-date 2019 3 1))
      (calendar-date 2019 3 25)
      :test #'calendar-date=)

  (is (nth-day-of-week-of-the-month 5 5 (calendar-date 2019 3 1))
      (calendar-date 2019 3 29)
      :test #'calendar-date=)

  (is-error (nth-day-of-week-of-the-month 0 1 (calendar-date 2019 3 1))
	    type-error
	    "invalid value.")

  (is-error (nth-day-of-week-of-the-month 5 1 (calendar-date 2019 3 1))
	    simple-error
	    "invalid value.")

  (is-error (nth-day-of-week-of-the-month 1 0 (calendar-date 2019 3 1))
	    type-error
	    "invalid day of week.")

  (is-error (nth-day-of-week-of-the-month 1 8 (calendar-date 2019 3 1))
	    type-error
	    "invalid day of week.")

  (is-error (nth-day-of-week-of-the-month 1 1 :foo)
	    type-error
	    "invalid calendar date."))

(subtest "next-month"

  (is (next-month (calendar-date 2015 1 1))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is (next-month (calendar-date 2015 1 2))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is-error (next-month :foo)
            type-error
            "invalid calendar date."))

(subtest "previous-month"

  (is (previous-month (calendar-date 2015 2 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (previous-month (calendar-date 2015 2 2))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is-error (previous-month :foo)
            type-error
            "invalid calendar date."))

(subtest "same-day-of-next-month"

  (is (same-day-of-next-month (calendar-date 2015 1 1))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is (same-day-of-next-month (calendar-date 2015 1 2))
      (calendar-date 2015 2 2)
      :test #'calendar-date=)

  (is (same-day-of-next-month (calendar-date 2015 1 31))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (same-day-of-next-month (calendar-date 2016 1 31))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (same-day-of-next-month :foo)
            type-error
            "invalid calendar date."))

(subtest "same-day-of-previous-month"

  (is (same-day-of-previous-month (calendar-date 2015 2 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (same-day-of-previous-month (calendar-date 2015 2 2))
      (calendar-date 2015 1 2)
      :test #'calendar-date=)

  (is (same-day-of-previous-month (calendar-date 2015 3 31))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (same-day-of-previous-month (calendar-date 2016 3 31))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (same-day-of-previous-month :foo)
            type-error
            "invalid calendar date."))

(subtest "first-of-the-month"

  (is (first-of-the-month (calendar-date 2015 1 2))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is-error (first-of-the-month :foo)
            type-error
            "invalid calendar date."))

(subtest "nth-of-the-month"

  (is (nth-of-the-month 1 (calendar-date 2015 1 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (nth-of-the-month 31 (calendar-date 2015 1 1))
      (calendar-date 2015 1 31)
      :test #'calendar-date=)

  (is (nth-of-the-month 28 (calendar-date 2015 2 1))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (nth-of-the-month 29 (calendar-date 2016 2 1))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (nth-of-the-month :foo (calendar-date 2015 1 1))
            type-error
            "invalid day.")

  (is-error (nth-of-the-month 0 (calendar-date 2015 1 1))
            type-error
            "invalid day.")

  (is-error (nth-of-the-month 32 (calendar-date 2015 1 1))
            type-error
            "invalid day.")

  (is-error (nth-of-the-month 29 (calendar-date 2015 2 1))
            simple-error
            "invalid day.")

  (is-error (nth-of-the-month 1 :foo)
            type-error
            "invalid calendar date."))

(subtest "nth-weekday-of-the-month"

  (is (nth-weekday-of-the-month 1 (calendar-date 2015 1 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (nth-weekday-of-the-month 3 (calendar-date 2015 1 1))
      (calendar-date 2015 1 5)
      :test #'calendar-date=)

  (is (nth-weekday-of-the-month 22 (calendar-date 2015 1 1))
      (calendar-date 2015 1 30)
      :test #'calendar-date=)

  (is-error (nth-weekday-of-the-month :foo (calendar-date 2015 1 1))
            type-error
            "invalid day.")

  (is-error (nth-weekday-of-the-month 0 (calendar-date 2015 1 1))
            type-error
            "invalid day.")

  (is-error (nth-weekday-of-the-month 23 (calendar-date 2015 1 1))
            simple-error
            "invalid day.")

  (is-error (nth-weekday-of-the-month 1 :foo)
            type-error
            "invalid calendar date."))

(subtest "last-day-of-the-month"

  (is (last-day-of-the-month (calendar-date 2015 1 1))
      (calendar-date 2015 1 31)
      :test #'calendar-date=)

  (is (last-day-of-the-month (calendar-date 2015 2 1))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (last-day-of-the-month (calendar-date 2016 2 1))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (last-day-of-the-month :foo)
            type-error
            "invalid calendar date."))

(subtest "last-weekday-of-the-month"

  (is (last-weekday-of-the-month (calendar-date 2015 1 1))
      (calendar-date 2015 1 30)
      :test #'calendar-date=)

  (is-error (last-weekday-of-the-month :foo)
            type-error
            "invalid calendar date."))

(subtest "nth-last-day-of-the-month"

  (is (nth-last-day-of-the-month 1 (calendar-date 2019 3 1))
      (calendar-date 2019 3 31)
      :test #'calendar-date=)

  (is (nth-last-day-of-the-month 2 (calendar-date 2019 3 1))
      (calendar-date 2019 3 30)
      :test #'calendar-date=)

  (is (nth-last-day-of-the-month 31 (calendar-date 2019 3 1))
      (calendar-date 2019 3 1)
      :test #'calendar-date=)

  (is-error (nth-last-day-of-the-month :foo (calendar-date 2019 3 1))
            type-error
            "invalid value.")

  (is-error (nth-last-day-of-the-month 0 (calendar-date 2019 3 1))
            type-error
            "invalid value.")

  (is-error (nth-last-day-of-the-month 32 (calendar-date 2019 3 1))
            type-error
            "invalid value.")

  (is-error (nth-last-day-of-the-month 29 (calendar-date 2019 2 1))
            simple-error
            "invalid value.")

  (is-error (nth-last-day-of-the-month 1 :foo)
            type-error
            "invalid calendar date."))

(subtest "nth-last-weekday-of-the-month"

  (is (nth-last-weekday-of-the-month 1 (calendar-date 2019 3 1))
      (calendar-date 2019 3 29)
      :test #'calendar-date=)

  (is (nth-last-weekday-of-the-month 21 (calendar-date 2019 3 1))
      (calendar-date 2019 3 1)
      :test #'calendar-date=)

  (is-error (nth-last-weekday-of-the-month :foo (calendar-date 2019 3 1))
            type-error
            "invalid value.")

  (is-error (nth-last-weekday-of-the-month 0 (calendar-date 2019 3 1))
            type-error
            "invalid value.")

  (is-error (nth-last-weekday-of-the-month 22 (calendar-date 2019 3 1))
            simple-error
            "invalid value.")

  (is-error (nth-last-weekday-of-the-month 1 :foo)
            type-error
            "invalid calendar date."))


(finalize)
