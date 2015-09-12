(in-package :cl-user)
(defpackage calendar-date-test
  (:use :cl
        :calendar-date
        :prove))
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

(subtest "last-day-of-the-month"

  (is (calendar-date::last-day-of-year-month 2015 1)
      31)

  (is (calendar-date::last-day-of-year-month 2015 4)
      30)

  (is (calendar-date::last-day-of-year-month 2015 2)
      28)

  (is (calendar-date::last-day-of-year-month 2016 2)
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
  )

(subtest "calendar-date-day-of-week"
  )

(subtest "calendar-date-values"
  )

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

(subtest "business-day-p"

  (is (business-day-p (calendar-date 2015 1 1))
      t)

  (is (business-day-p (calendar-date 2015 1 3))
      nil)

  (is (business-day-p (calendar-date 2015 1 4))
      nil)

  (is-error (business-day-p :foo)
            type-error
            "invalid calendar date."))

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

(subtest "print-object"

  (is-print (princ (calendar-date 2015 1 1))
            "#<CALENDAR-DATE 2015-01-01>")

  (is-print (prin1 (calendar-date 2015 1 1))
            "#<CALENDAR-DATE 2015-01-01>"))

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

(subtest "next-week"
  )

(subtest "previous-week"
  )

(subtest "next-month"

  (is (next-month (calendar-date 2015 1 1))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is (next-month (calendar-date 2015 1 31))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (next-month (calendar-date 2016 1 31))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (next-month :foo)
            type-error
            "invalid calendar date."))

(subtest "previous-month"

  (is (previous-month (calendar-date 2015 2 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (previous-month (calendar-date 2015 3 31))
      (calendar-date 2015 2 28)
      :test #'calendar-date=)

  (is (previous-month (calendar-date 2016 3 31))
      (calendar-date 2016 2 29)
      :test #'calendar-date=)

  (is-error (previous-month :foo)
            type-error
            "invalid calendar date."))

(subtest "first-of-the-month"

  (is (first-of-the-month (calendar-date 2015 1 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is-error (first-of-the-month :foo)
            type-error
            "invalid calendar date."))

(subtest "first-of-next-month"

  (is (first-of-next-month (calendar-date 2015 1 1))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is-error (first-of-next-month :foo)
            type-error
            "invalid calendar date."))

(subtest "first-of-previous-month"
  )

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

(subtest "nth-of-the-month-in-business"

  (is (nth-of-the-month 1 (calendar-date 2015 1 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is (nth-of-the-month-in-business 3 (calendar-date 2015 1 1))
      (calendar-date 2015 1 2)
      :test #'calendar-date=)

  (is (nth-of-the-month-in-business 4 (calendar-date 2015 1 1))
      (calendar-date 2015 1 2)
      :test #'calendar-date=))

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


(finalize)
