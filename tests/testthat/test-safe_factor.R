test_that("output values match factor()/ordered() for good input", {
  x <- c("a", "b", "c", NA, "b")
  lvls <- c("c", "b", "a")
  # Levels work as expected
  expect_equal(
    safe_factor(x, levels = lvls),
    factor(x, levels = lvls)
  )

  expect_equal(
    safe_ordered(x, levels = lvls),
    ordered(x, levels = lvls)
  )
})

test_that("ordered inputs are returned as ordered", {
  expect_s3_class(safe_factor(ordered(c("a", "b")), levels = c("b", "a")),
                  "ordered")
})

test_that("unmatched values trigger an error", {
  expect_error(safe_factor(c("a", "b"), levels = c("a", "c")),
               "not match provided levels")

  expect_error(safe_ordered(c("a", "b"), levels = c("a", "c")),
               "not match provided levels")
})

test_that("works with no input vector", {
  expect_equal(
    safe_factor(levels = c("a", "b")),
    factor(levels = c("a", "b"))
  )

  expect_equal(
    safe_ordered(levels = c("a", "b")),
    ordered(levels = c("a", "b"))
  )
})

test_that("Works with no set levels", {
  expect_equal(
    safe_factor(c("a", "b")),
    factor(c("a", "b"))
  )

  expect_equal(
    safe_ordered(c("a", "b")),
    ordered(c("a", "b"))
  )
})

test_that("doesn't ignore case by default", {
  expect_error(safe_factor(c("a", "b"), levels = c("a", "B")),
               "not match provided levels")

  expect_error(safe_ordered(c("a", "b"), levels = c("a", "B")),
               "not match provided levels")
})

test_that("updates case when directed", {
  expect_equal(safe_factor(c("test", "Test", "TeSt", "ing", "inG"),
                           levels = c("TEST", "ING"),
                           update.case = TRUE),
              factor(c("TEST", "TEST", "TEST", "ING", "ING"),
                     levels = c("TEST", "ING")))

  expect_equal(safe_ordered(c("test", "Test", "TeSt", "ing", "inG"),
                            levels = c("TEST", "ING"),
                            update.case = TRUE),
               ordered(c("TEST", "TEST", "TEST", "ING", "ING"),
                       levels = c("TEST", "ING")))
})

