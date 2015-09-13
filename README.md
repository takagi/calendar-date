# Calendar-date

**The project is planed to be on top of LOCAL-TIME as its extension later.**

[![Circle CI](https://circleci.com/gh/takagi/calendar-date/tree/master.svg?style=shield)](https://circleci.com/gh/takagi/calendar-date/tree/master)
[![Coverage Status](https://coveralls.io/repos/takagi/calendar-date/badge.svg?branch=master&service=github)](https://coveralls.io/github/takagi/calendar-date?branch=master)

Calendar-date is a Gregorian calendar date library in Common Lisp.

## Usage

## Installation

**Since calendar-date is not on Quicklisp yet, please use its local-projects feature for now.**

    cd quicklisp/local-projects
    git clone git@github.com:takagi/calendar-date

You can install calendar-date via Quicklisp,

    (ql:quickload :calendar-date)

## API

### [Function] calendar-date

    CALENDAR-DATE year month day => new-calendar-date

### [Function] calendar-date-today

    CALENDAR-DATE-TODAY &key timezone => new-calendar-date

### [Function] calendar-date-year

    CALENDAR-DATE-YEAR calendar-date => year

### [Function] calendar-date-month

    CALENDAR-DATE-MONTH calendar-date => month

### [Function] calendar-date-day

    CALENDAR-DATE-DAY calendar-date => day

### [Function] calendar-date-values

    CALENDAR-DATE-VALUES calendar-date => year, month, day

### [Function] calendar-date-day-of-week

    CALENDAR-DATE-DAY-OF-WEEK calendar-date => day-of-week

### [Function] calendar-date=, calendar-date/=, calendar-date<, calendar-date>, calendar-date<=, calendar-date>=

    CALENDAR-DATE= calendar-date1 calendar-date2 => generalized-boolean
    CALENDAR-DATE/= calendar-date1 calendar-date2 => generalized-boolean
    CALENDAR-DATE< calendar-date1 calendar-date2 => generalized-boolean
    CALENDAR-DATE> calendar-date1 calendar-date2 => generalized-boolean
    CALENDAR-DATE<= calendar-date1 calendar-date2 => generalized-boolean
    CALENDAR-DATE>= calendar-date1 calendar-date2 => generalized-boolean

### [Function] business-day-p

    BUSINESS-DAY-P calendar-date => generalized-boolean

### [Function] weekday-p

    WEEKDAY-P calendar-date => generalized-boolean

### [Function] weekend-p

    WEEKEND-P calendar-date => generalized-boolean

### [Function] next-day

    NEXT-DAY calendar-date => new-calendar-date

### [Function] previous-day

    PREVIOUS-DAY calendar-date => new-calendar-date

### [Function] next-week

    NEXT-WEEK calendar-date => new-calendar-date

### [Function] previous-week

    PREVIOUS-WEEK calendar-date => new-calendar-date

### [Function] same-day-of-week-of-next-week

    SAME-DAY-OF-WEEK-OF-NEXT-WEEK calendar-date => new-calendar-date

### [Function] same-day-of-week-of-previous-week

    SAME-DAY-OF-WEEK-OF-PREVIOUS-WEEK calendar-date => new-calendar-date

### [Function] day-of-week-of-the-week

    DAY-OF-WEEK-OF-THE-WEEK day-of-week calendar-date => new-calendar-date

### [Function] next-month

    NEXT-MONTH calendar-date => new-calendar-date

### [Function] previous-month

    PREVIOUS-MONTH calendar-date => new-calendar-date

### [Function] same-day-of-next-month

    SAME-DAY-OF-NEXT-MONTH calendar-date => new-calendar-date

### [Function] same-day-of-previous-month

    SAME-DAY-OF-PREVIOUS-MONTH calendar-date => new-calendar-date

### [Function] first-of-the-month

    FIRST-OF-THE-MONTH calendar-date => new-calendar-date

### [Function] nth-of-the-month

    NTH-OF-THE-MONTH nth calendar-date => new-calendar-date

### [Function] nth-of-the-month-in-business

    NTH-OF-THE-MONTH-IN-BUSINESS nth calendar-date => new-calendar-date

### [Function] nth-business-day-of-the-month

    NTH-BUSINESS-DAY-OF-THE-MONTH nth calendar-date => new-calendar-date

### [Function] last-day-of-the-month

    LAST-DAY-OF-THE-MONTH calendar-date => new-calendar-date

### [Function] last-business-day-of-the-month

    LAST-BUSINESS-DAY-OF-THE-MONTH calendar-date => new-calendar-date

## Author

* Masayuki Takagi (kamonama@gmail.com)

## Copyright

Copyright (c) 2015 Masayuki Takagi (kamonama@gmail.com)

## License

Licensed under the MIT License.
