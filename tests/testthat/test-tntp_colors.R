test_that("choose_text_color() matches the names of bg_color", {
  a <- c("Yes" = "green", "No" = "red")
  b <- unname(a)
  expect_equal(names(choose_text_color(a)), names(a))
  expect_equal(names(choose_text_color(b)), names(b))
})

test_that("choose_text_color() works as expected for basic colors", {
  expect_equal(choose_text_color(c("#111111", "#DDDDDD")), c("white", "black"))
})

test_that("choose_text_color() handles edge cases as expected", {
  expect_equal(choose_text_color("#111111"), "white")
  expect_equal(choose_text_color("white", text_colors = "blue"), "blue")
  expect_equal(choose_text_color(list()), list())
  expect_equal(choose_text_color(NULL), NULL)

  expect_error(choose_text_color("red", text_colors = NULL), "No text colors provided")
  expect_error(choose_text_color("red", text_colors = list()), "No text colors provided")
  expect_error(choose_text_color("a"), "must be a vector of colors")
  expect_error(choose_text_color("red", text_colors = "a"), "must be a vector of colors")
  expect_error(choose_text_color("white", text_colors = "yellow"), "No high-contrast text color")
})

test_that("tntp_colors returns duplicated colors", {
  expect_equal(tntp_colors("navy", "navy"), c(tntp_colors("navy"), tntp_colors("navy")))
})

test_that("tntp_colors returns colors in the correct order", {
  expect_equal(tntp_colors("navy", "mint"), rev(tntp_colors("mint", "navy")))
})

test_that("tntp_colors and tntp_palette use inputted names when run with arguments", {
  # No names if names aren't provided
  expect_equal(tntp_colors("navy"), tntp_colors("navy") |> unname())
  expect_equal(tntp_colors("navy", "mint"), tntp_colors("navy", "mint") |> unname())
  expect_equal(tntp_palette("likert_4"), tntp_palette("likert_4") |> unname())

  # Uses provided names if given
  expect_equal(tntp_colors(a = "gold"), tntp_colors("gold") |> rlang::set_names("a"))
  expect_equal(tntp_colors("y" = "navy", "n" = "gold"),
               tntp_colors("navy", "gold") |> rlang::set_names("y", "n"))
})

test_that("tntp_colors returns named vector when run empty", {
  expect_equal(length(names(tntp_colors())), length(tntp_colors()))
})

test_that("tntp_colors returns no duplicates when run empty", {
  expect_equal(unname(tntp_colors()), unique(tntp_colors()))
})

test_that("tntp_colors and tntp_palette raise an error for unmatched colors or palettes", {
  expect_error(tntp_colors("notacolor"), "No match")
  expect_error(show_tntp_colors("notacolor"), "No match")
  expect_error(tntp_palette("notapalette"), "No TNTP palette found")
  expect_error(show_tntp_palette("notapalette"), "No match")
})

test_that("choose_text_color works as expected for very light and dark colors", {
  expect_equal(choose_text_color("#111111"), "white")
  expect_equal(choose_text_color("#EEEEEE"), "black")
})

test_that("is_color recognizes common words and hex values", {
  expect_equal(is_color("blue"), TRUE)
  expect_equal(is_color("#FFFFFF"), TRUE)
  expect_equal(is_color("notacolor"), FALSE)
  expect_equal(is_color("#BAD10"), FALSE)
})

test_that("tntp_palette reverse works as expected", {
  expect_equal(rev(tntp_palette()), tntp_palette(reverse = TRUE))
})

test_that("show_tntp_colors and show_tntp_palette parameter validation is working", {
  expect_warning(show_tntp_colors(labels = "yes"), "Invalid")
  expect_warning(show_tntp_colors(borders = "notacolor"), "Invalid")
  expect_error(show_tntp_palette(pattern = FALSE), "No palettes")
  expect_error(show_tntp_colors(pattern = FALSE), "No colors")
})
