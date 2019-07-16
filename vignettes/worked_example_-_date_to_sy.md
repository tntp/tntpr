Worked Example: `tntpr::date_to_sy`
================
2019-07-16

  - `date_to_sy`: Checks to see if a date is past the user-specified
    cutoff point for delineating school years, then maps to the
    appropriate year.

Say you have a test date and you want to create a new variable that
tells you the school year the test was taken.

    ## # A tibble: 100 x 2
    ##    student_id test_date 
    ##         <int> <date>    
    ##  1          1 2013-01-30
    ##  2          2 2014-02-01
    ##  3          3 2015-12-23
    ##  4          4 2019-02-17
    ##  5          5 2012-06-24
    ##  6          6 2019-01-12
    ##  7          7 2019-06-19
    ##  8          8 2016-10-17
    ##  9          9 2016-06-29
    ## 10         10 2011-03-01
    ## # ... with 90 more rows

Historically, I would have used a long, error-prone `case_when`
mutation, but the `tntpr::date_to_sy` function is much easier.

The function takes two arguments, `date_var` and `last_day_of_sy` (year
doesn’t matter) and returns a character string with the school year in
the form ‘(year) - (year)’.

``` r
appl_dat %>% 
  mutate(hire_date_sy = date_to_sy(test_date, last_day_of_sy = ymd("2018-06-01")))
```

    ## # A tibble: 100 x 3
    ##    student_id test_date  hire_date_sy
    ##         <int> <date>     <chr>       
    ##  1          1 2013-01-30 2012 - 2013 
    ##  2          2 2014-02-01 2013 - 2014 
    ##  3          3 2015-12-23 2015 - 2016 
    ##  4          4 2019-02-17 2018 - 2019 
    ##  5          5 2012-06-24 2012 - 2013 
    ##  6          6 2019-01-12 2018 - 2019 
    ##  7          7 2019-06-19 2019 - 2020 
    ##  8          8 2016-10-17 2016 - 2017 
    ##  9          9 2016-06-29 2016 - 2017 
    ## 10         10 2011-03-01 2010 - 2011 
    ## # ... with 90 more rows
