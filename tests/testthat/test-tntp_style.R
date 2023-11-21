

test_that("tntp_style parameter validation is working", {
  expect_warning(tntp_style(base_size = 5), "not recommended")

  expect_error(tntp_style(base_size = FALSE), "Invalid")
})

test_that("tntp_style font validation works", {
  thm <- expect_warning(tntp_style(family = "notafamily"), "not registered")
  expect_warning(tntp_style(header_family = "notafamily"), "not registered")

  thm <- suppressWarnings(tntp_style(family = "notafamily"))
  families <- unique(c(thm$axis.text$family,
                       thm$legend.text$family,
                       thm$plot.title$family,
                       thm$plot.subtitle$family,
                       thm$plot.caption$family,
                       thm$strip.text$family))
  expect_equal(families, "sans")

  thm <- suppressWarnings(tntp_style(family = "serif"))
  families <- unique(c(thm$axis.text$family,
                       thm$legend.text$family,
                       thm$plot.title$family,
                       thm$plot.subtitle$family,
                       thm$plot.caption$family,
                       thm$strip.text$family))
  expect_equal(families, "serif")
})

test_that("tntp_style is a theme", {
  expect_s3_class(tntp_style(), "theme")
})
