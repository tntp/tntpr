test_that("bar_chart_counts accurately summarizes data", {

  df <- tibble::tibble(col = c("c", "d", "e", "e", "d", "c", "c"),
                       grp = c("a", "b", "a", "b", "a", "a", "a"),
                       val = c(1, 2, 3, 4, 5, 6, 7))

  plt <- bar_chart_counts(df, col)
  exp <- tibble::tibble(
    vec.factor = factor(c("c", "d", "e"), levels = c("c", "d", "e")),
    n = c(3, 2, 2),
    perc = c(3/7, 2/7, 2/7))
  expect_equal(plt$data, exp)

})

test_that("bar_chart_counts grouping variable works", {

  df <- tibble::tibble(col = c("c", "d", "e", "e", "d", "c", "c"),
                       grp = c("a", "b", "a", "b", "a", "a", "a"),
                       val = c(1, 2, 3, 4, 5, 6, 7))

  plt <- bar_chart_counts(df, col, grp)
  exp <- tibble::tibble(
    vec.factor = factor(rep(c("c", "d", "e"), each = 2), levels = c("c", "d", "e")),
    group.factor = factor(rep(c("a", "b"), 3), levels = c("a", "b")),
    n = c(3, NA, 1, 1, 1, 1),
    perc = c(3/3, NA, 1/2, 1/2, 1/2, 1/2))
  expect_equal(plt$data, exp)

})
