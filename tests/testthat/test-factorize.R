# Example data frame for testing
x <- data.frame(
  a = c("a lot", "a little", "some", "some"),
  b = 1:4,
  c = rep(as.POSIXct(Sys.Date()), 4),
  stringsAsFactors = FALSE
)

# Tests
test_that("works with POSIX (two-class) columns present", {
  y <- x
  y$a <- factor(y$a, c("a little", "some", "a lot"))
  expect_equal(tibble::as_tibble(y), factorize_df(x, c("a little", "some", "a lot")))
})

test_that("warning message if no matches found", {
  expect_warning(factorize_df(x, c("bugs", "birds")),
                 "No columns matched.",
                 fixed = TRUE
  )
})

test_that("appropriate message prints if matched but already factor", {
  aa <- suppressMessages(factorize_df(x, 4:1))
  ab <- aa
  ab$d <- 1:4

  factorize_df(aa, 4:1) |>
    suppressMessages() |>
    expect_warning("matching column was already a factor") |>
    expect_warning("No new columns matched.") # Includes "new " in message

  # the unusual case where there's a match on a non-factor and an already-present factor match
  factorize_df(ab, 4:1) |>
    expect_warning("matching column was already a factor") |>
    expect_message("Changed the following columns to factors:")
})
