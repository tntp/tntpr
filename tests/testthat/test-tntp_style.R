

test_that("get_usable_family returns a usable family", {
  expect_equal(get_usable_family('serif'), 'serif')
  expect_equal(suppressWarnings(get_usable_family('notafamily')), 'sans')
})

test_that("get_usable_family errors and warnings work as expected", {
  expect_error(get_usable_family('notafamily', default_family = 'alsonotafamily'), "not registered")
  expect_error(get_usable_family('notafamily', default_family = 'alsonotafamily', silent = TRUE), "not registered")
  expect_warning(get_usable_family('notafamily'), "not registered")
  expect_no_warning(get_usable_family('notafamily', silent = TRUE))
})

test_that("get_usable_family default_family works as expected", {
  expect_equal(suppressWarnings(get_usable_family('notafamily', default_family = 'serif')), 'serif')
})


test_that("tntp_style parameter validation is working", {
  expect_warning(tntp_style(base_size = 5), "not recommended")

  expect_error(tntp_style(base_size = FALSE), "Invalid")
})

test_that("tntp_style font validation works", {
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
