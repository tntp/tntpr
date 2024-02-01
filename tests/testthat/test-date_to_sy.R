
test_that("date_to_sy works", {

  test_dates <- c(as.Date("2013-10-10"),
                  as.Date("2013-10-11"),
                  as.Date("2013-10-12"))

  exp <-  c("2012 - 2013",
            "2012 - 2013",
            "2013 - 2014")

  expect_equal(date_to_sy(test_dates, as.Date("2010-10-11")), exp)

})

test_that("date_to_sy defaults to a cutoff date of 7/1", {
  test_dates <- c("6/30/19",
                  "7/1/19",
                  "7/2/19")

  exp <- c("2018 - 2019",
           "2018 - 2019",
           "2019 - 2020")

  expect_equal(suppressMessages(date_to_sy(test_dates)), exp)


  expect_message(date_to_sy(as.Date("2013-01-01")), "Using a default cutoff date")

})

test_that("parse_date works for different date formats", {

  d <- as.Date("2013-01-03")

  suppressMessages({
    expect_equal(parse_date("2013-1-3"), d)
    expect_equal(parse_date("1/3/2013"), d)
    expect_equal(parse_date(d), d)
    expect_equal(parse_date("1/3/13"), d)
    expect_equal(parse_date("13-1-3"), d)
  })

})

test_that("parse_date works for character vectors", {

  dates <- c("1/1/13", "1/2/13", "1/3/13")
  exp <- as.Date(c("2013-01-01", "2013-01-02", "2013-01-03"))

  expect_equal(suppressMessages(parse_date(dates)), exp)
})

test_that("parse_date is noisy when parsing, and silent when not", {
  expect_message(parse_date("2013-1-3"), "Parsed")
  expect_no_message(parse_date(as.Date("2013-01-013")))
})

test_that("parse_date fails with bad input", {
  expect_error(parse_date("notadate"), "Could not parse")
})
