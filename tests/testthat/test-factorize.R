# Example data frame for testing
x <- data.frame(
  a = c("a lot", "a little", "some", "some"),
  b = 1:4,
  c = rep(as.POSIXct(Sys.Date()), 4),
  stringsAsFactors = FALSE
)

# Tests
test_that("prop_matching works as expected", {
  a <- c("frog", "lizard", "frog", "frog")
  b <- c("FroG", "LIZard", "FROG", "frog")
  c <- c("frog", "bunny", "frog", "lizard")

  valid <- c("frog", "lizard")

  expect_equal(prop_matching(a, valid), 1)
  expect_equal(prop_matching(b, valid, ignore.case = FALSE), 0.25)
  expect_equal(prop_matching(b, valid, ignore.case = TRUE), 1)
  expect_equal(prop_matching(c, valid), 0.75)
})

test_that("standardize_case works as expected", {
  a <- c("yes", "no", "Yes", "Yess", "NO")
  expect_equal(standardize_case(a, "Yes"), c("Yes", "no", "Yes", "Yess", "NO"))
  expect_equal(standardize_case(a, c("YES", "NO")), c("YES", "NO", "YES", "Yess", "NO"))
})

test_that("ignore.case works", {
  df <- tibble::tibble(
    a = c("Yes", "yes", "No", "nO"),
    b = c("Yes", "yes", "yes", "YES"),
    c = c("yes", "maybe", "no", "no")
  )

  ymn <- c("yes", "maybe", "no")
  YN <- c("Yes", "No")

  # Warning when not provided:
  expect_equal(factorize_df(df, ymn), df |> dplyr::mutate(c = factor(c, levels = ymn))) |>
    expect_warning("NOT matched, but would match if") |>
    suppressMessages()

  # No warning when set to FALSE
  expect_equal(factorize_df(df, ymn, ignore.case = FALSE), df |> dplyr::mutate(c = factor(c, levels = ymn))) |>
    suppressMessages()

  # Ignores case and no warning when set to TRUE:
  factorize_df(df, YN, ignore.case = TRUE) |>
    expect_equal(df |> dplyr::mutate(a = factor(c("Yes", "Yes", "No", "No"), levels = YN),
                                     b = factor(c("Yes", "Yes", "Yes", "Yes"), levels = YN))) |>
    suppressMessages()
})

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
