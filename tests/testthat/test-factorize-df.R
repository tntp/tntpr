# Tests for factorizing columns based on string matching

library(tntpr); library(testthat)
context("factorize_df")

x <- data.frame(a = c("a lot", "a little", "some", "some"),
                b = 1:4,
                c = rep(as.POSIXct(Sys.Date()), 4),
                stringsAsFactors = FALSE)

test_that("works with POSIX (two-class) columns present", {
  y <- x
  y$a <- factor(y$a, c("a little", "some", "a lot"))
  expect_equal(tibble::as_tibble(y), factorize_df(x, c("a little", "some", "a lot")))
})

test_that("warning message if no matches found", {
  expect_warning(factorize_df(x, c("bugs", "birds")),
                 "No columns matched.  Check spelling & capitalization of your levels.",
                 fixed = TRUE)
})

test_that("appropriate message prints if matched but already factor", {
  aa <- factorize_df(x, 4:1)
  ab <- aa
  ab$d <- 1:4
  expect_warning(suppressMessages(factorize_df(aa, 4:1)),
                 "at least one matching column was already a factor, though this call will have reordered it if different levels provided.  Could be caused by overlapping sets of factor levels, e.g., \"Yes\", \"Maybe\", \"No\" and \"Yes\", \"No\".",
                 fixed = TRUE)

  expect_warning(suppressMessages(factorize_df(aa, 4:1)), # it says no NEW columns, acknowledging that there was a match but it was already a factor
                 "No new columns matched.  Check spelling & capitalization of your levels.",
                 fixed = TRUE)

  # the unusual case where there's a match on a non-factor and an already-present factor match
  expect_warning(factorize_df(ab, 4:1),
                 "at least one matching column was already a factor, though this call will have reordered it if different levels provided.  Could be caused by overlapping sets of factor levels, e.g., \"Yes\", \"Maybe\", \"No\" and \"Yes\", \"No\".",
                 fixed = TRUE)
  expect_message(suppressWarnings(factorize_df(ab, 4:1)),
                 paste("Transformed these columns: \n", paste("* ", "d", collapse = ", \n")),
                 fixed = TRUE)

})
