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

test_that("swap_colors provides TNTP colors if possible", {
  expect_equal(swap_colors(c('navy', 'mint')), tntp_colors('navy', 'mint'))
})

test_that("swap_colors uses R colors if TNTP colors aren't found", {
  expect_message(swap_colors(c("brown", "orange")), "Unable to map some colors.* to TNTP colors")
  expect_equal(suppressMessages(swap_colors(c("brown", "orange"))), c("brown", "orange"))
})

test_that("swqp_colors raises a warning and uses the colorful TNTP palette if no mapping is found", {
  expect_warning(swap_colors(c("blue", "notacolor")), "Unable to map some colors")
  expect_equal(suppressWarnings(swap_colors(c("blue", "notacolor"))), tntp_palette('colorful'))
})
