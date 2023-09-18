# Tests for date-to-school year function

library(dplyr)
library(lubridate)
context("date_to_sy")

test_that("conversion is accurate", {
  expect_equal(date_to_sy(as.Date("2014-05-05"), as.Date("2000-07-01")), "2013 - 2014")
  expect_equal(date_to_sy(as.Date("2014-07-05"), as.Date("2000-07-01")), "2014 - 2015")
})

test_that("year in cutoff date doesn't matter", {
  expect_equal(
    date_to_sy(as.Date("2014-05-05"), as.Date("2000-07-01")),
    date_to_sy(as.Date("2014-05-05"), as.Date("1234-07-01"))
  )
})
