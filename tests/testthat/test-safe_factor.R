test_that("output values match factor()/ordered() for good input", {
  # Levels work as expected
  expect_equal(
    safe_factor(c("a", "b", "c", NA, "b"), levels = c("c", "b", "a")),
    factor(c("a", "b", "c", NA, "b"), levels = c("c", "b", "a"))
  )

  expect_equal(
    safe_ordered(c("a", "b", "c", NA, "b"), levels = c("c", "b", "a")),
    ordered(c("a", "b", "c", NA, "b"), levels = c("c", "b", "a"))
  )
})

test_that("ordered inputs are returned as ordered", {
  expect_s3_class(safe_factor(ordered(c("a", "b")), levels = c("b", "a")),
                  "ordered")
})

test_that("unmatched values trigger an error", {
  expect_error(safe_factor(c("a", "b"), levels = c("a", "c")),
               "does not match provided levels")

  expect_error(safe_ordered(c("a", "b"), levels = c("a", "c")),
               "does not match provided levels")

  # Checks for casing
  expect_error(safe_factor(c("a", "b"), levels = c("a", "B")),
               "does not match provided levels")
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
