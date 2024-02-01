test_that("theme_tntp_2018 is a theme", {
  expect_s3_class(theme_tntp_2018(), "theme") |>
    expect_warning("deprecated")
})
