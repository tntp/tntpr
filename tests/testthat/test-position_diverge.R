test_that("data keeps its order after stacking", {
  df <- data.frame(
    x = rep(c(1:10), 3),
    var = factor(rep(c("a", "b", "c"), 10)),
    y = round(runif(30, 1, 5))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = var)) +
    ggplot2::geom_area(stat = "identity", position = position_diverge())

  dat <- ggplot2::layer_data(p)
  expect_true(all(dat$group == rep(1:3, each = 10)))
  expect_true(all(dat$x == df$x))
})

test_that("negative plotting values are coerced to positive with warning", {
  df <- data.frame(
    x = c(1,  1, 1, 2,2,2),
    g = c(1,  2, 3, 1,2,3),
    y = c(-1,-1,-1,-2,2,3)
  )
  df2 <- data.frame(lapply(df, abs))

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = factor(g))) +
    ggplot2::geom_col(position = position_diverge())

  p2 <- ggplot2::ggplot(df2, ggplot2::aes(x, y, fill = factor(g))) +
    ggplot2::geom_col(position = position_diverge())

  expect_warning(print(p), "negative plotting values")

  # Check that data matches the positive data frame
  expect_equal(suppressWarnings(ggplot2::layer_data(p)),
               ggplot2::layer_data(p2))
})

test_that("can request reverse stacking", {
  df <- data.frame(
    y = c(2, 1),
    g = factor(c("a","b"))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(1, y, fill = g)) +
    ggplot2::geom_col(position = position_diverge(reverse = TRUE))
  dat <- ggplot2::layer_data(p)
  expect_equal(dat$ymin, c(-2, 0))
})

test_that("vjust works correctly with text", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(40, 20, 30, 10),
    group = factor(letters[1:4])
  )
  base <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = group))
  p0 <- base + ggplot2::geom_text(ggplot2::aes(label = y),
                                  position = position_diverge(vjust = 0))
  p1 <- base + ggplot2::geom_text(ggplot2::aes(label = y),
                                  position = position_diverge(vjust = 1))
  p.5 <- base + ggplot2::geom_text(ggplot2::aes(label = y),
                                  position = position_diverge(vjust = 0.5))

  expect_equal(ggplot2::layer_data(p0)$y, c(20, 0, -30, -40))
  expect_equal(ggplot2::layer_data(p1)$y, c(60, 20, 0, -30))
  expect_equal(ggplot2::layer_data(p.5)$y, c(40, 10, -15, -35))
})

test_that("fill = TRUE correctly scales values", {
  df <- data.frame(
    x = c(1, 1, 2, 2),
    y = c(0.1, 0.3, 4, 6),
    g = factor(c("a", "b", "a", "b"))
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = position_diverge(fill = TRUE))

  dat <- ggplot2::layer_data(p)

  expect_equal(dat$ymax, c(0.25, 0, 0.4, 0))
  expect_equal(dat$ymin, c(0, -0.75, 0, -0.6))
})

test_that("break_after accepts NULL, character, or integer values", {
  df <- data.frame(
    x <- 1,
    y <- c(1, 2, 3, 4, 5, 6),
    g <- factor(letters[1:6])
  )

  p_null <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = position_diverge(break_after = NULL))
  p_int <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = position_diverge(break_after = 4))
  p_chr <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = g)) +
    ggplot2::geom_col(position = position_diverge(break_after = "e"))

  dat_null <- ggplot2::layer_data(p_null)
  dat_int  <- ggplot2::layer_data(p_int)
  dat_chr  <- ggplot2::layer_data(p_chr)

  expect_equal(dat_null$ymax, c(6, 5, 3, 0, -4, -9))
  expect_equal(dat_int$ymax, c(10, 9, 7, 4, 0, -5))
  expect_equal(dat_chr$ymax, c(15, 14, 12, 9, 5, 0))
})

test_that("missing factor levels are handled correctly", {
  df <- data.frame(
    x = c(1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7),
    y = c(1, 2, 4, 1, 3, 4, 1, 4, 1, 2, 3, 4, 1, 4)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = factor(y))) +
    ggplot2::geom_col(position = position_diverge())

  dat <- ggplot2::layer_data(p)
  expect_equal(dat$ymax, c(3, 2, 0, 1, 0, -3, 1, 0, 3, 2, 0, -3, 1, 0))
})
