(in-package :cl-user)
(defpackage calendar-date-test
  (:use :cl
        :calendar-date
        :prove))
(in-package :calendar-date-test)


(plan nil)

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

(subtest "beginning-of-the-month"

  (is (calendar-date::beginning-of-the-month (calendar-date 2015 1 1))
      (calendar-date 2015 1 1)
      :test #'calendar-date=)

  (is-error (calendar-date::beginning-of-the-month :foo)
            type-error
            "invalid calendar date."))

(subtest "beginning-of-next-month"

  (is (beginning-of-next-month (calendar-date 2015 1 1))
      (calendar-date 2015 2 1)
      :test #'calendar-date=)

  (is-error (beginning-of-next-month :foo)
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

(subtest "nth-of-the-month-in-business"

  )

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
