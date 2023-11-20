test_that("returns duplicated colors", {
  expect_equal(tntp_colors("navy", "navy"), c(tntp_colors("navy"), tntp_colors("navy")))
})

test_that("returns colors in the correct order", {
  expect_equal(tntp_colors("navy", "mint"), rev(tntp_colors("mint", "navy")))
})

test_that("returned vector is unnamed when run with arguments", {
  expect_equal(tntp_colors("navy"), tntp_colors("navy") |> unname())
  expect_equal(tntp_colors("navy", "mint"), tntp_colors("navy", "mint") |> unname())
  expect_equal(tntp_palette("likert_4"), tntp_palette("likert_4") |> unname())
})

test_that("returns named vector when run empty", {
  expect_equal(length(names(tntp_colors())), length(tntp_colors()))
})

test_that("returns no duplicates when run empty", {
  expect_equal(unname(tntp_colors()), unique(tntp_colors()))
})

test_that("raises an error for unmatched colors", {
  expect_error(tntp_colors("notacolor"), "No match for the following color name")
})
