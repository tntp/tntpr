test_that("theme_tntp is a theme", {
  expect_s3_class(theme_tntp(), "theme") |>
    expect_warning("deprecated")
})
