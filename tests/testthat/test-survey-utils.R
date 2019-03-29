# Tests for survey analysis functions

library(testthat)
library(janitor)
library(labelled)
library(dplyr)
context("survey utilities")


x <- data.frame( # 4th person didn't respond at all
  unrelated = 1:5, # not part of the question
  q1_1 = c("a", "a", NA, NA, NA),
  q1_2 = c("b", "b", NA, NA, NA),
  q1_3 = c(NA, NA, "c", NA, NA),
  q1_4 = c(NA, NA, NA, NA, NA), # no one selected this choice
  q1_other = c("horses", NA, NA, NA, "only other for this person"),
  stringsAsFactors = FALSE
)


# First, test without "other" column
treated_x <- x %>%
  check_all_recode(q1_1:q1_4)

tabulated_x <- x %>%
  check_all_recode(q1_1:q1_4) %>%
  check_all_count(q1_1:q1_4)

test_that("treatment performs as expected, including setting labels", {
  approx <- data.frame(
    unrelated = 1:5,
    q1_1 = c(1, 1, 0, NA, NA),
    q1_2 = c(1, 1, 0, NA, NA),
    q1_3 = c(0, 0, 1, NA, NA),
    q1_4 = c(rep(0, 3), NA, NA),
    q1_other = c("horses", NA, NA, NA, "only other for this person"),
    stringsAsFactors = FALSE
  )
  var_label(approx) <- list(unrelated = NULL, q1_1 = "a", q1_2 = "b", q1_3 = "c", q1_4 = NA_character_) # need to add variable labels to match the result of treatment

expect_equal(treated_x,  approx)

})

test_that("tabulation performs as expected", {
  expect_equal(
    tabulated_x,
    data.frame(
      response = c("a", "b", "c", "q1_4"),
      n = c(2, 2, 1, 0),
      percent = c(2/3, 2/3, 1/3, 0),
      stringsAsFactors = FALSE
      ) %>%
      as_tabyl(., 1)
  )})

## Now test with "other" column

treated_x_other <- suppressWarnings(
  check_all_recode(x,
                   contains("q1")))

expect_warning(x %>%
                 check_all_recode(contains("q1")), "has multiple values")

tabulated_x_other <- treated_x_other %>%
  check_all_count(contains("q1"))

test_that("treatment performs as expected, including setting labels", {
  approx_other <- data.frame(
    unrelated = 1:5,
    q1_1 = c(1, 1, 0, NA, 0),
    q1_2 = c(1, 1, 0, NA, 0),
    q1_3 = c(0, 0, 1, NA, 0),
    q1_4 = c(rep(0, 3), NA, 0),
    q1_other = c(1, 0, 0, NA, 1),
    stringsAsFactors = FALSE
  )
  var_label(approx_other) <- list(unrelated = NULL, q1_1 = "a", q1_2 = "b", q1_3 = "c", q1_4 = NA_character_, q1_other = "Other")

  expect_equal(treated_x_other, approx_other)

})

test_that("tabulation performs as expected", {
  expect_equal(tabulated_x_other, data.frame(
    response = c("a", "b", "c", "q1_4", "Other"),
    n = c(2, 2, 1, 0, 2),
    stringsAsFactors = FALSE
  ) %>%
    mutate(percent = n / 4) %>%
    as_tabyl(., 1))
})

test_that("select helpers work", {
  expect_equal(suppressWarnings(x %>% check_all_recode(q1_1:q1_other) %>% check_all_count(q1_1:q1_other)),
               suppressWarnings(x %>% check_all_recode(contains("q1")) %>% check_all_count(contains("q1"))))
})

test_that("bad inputs are caught", {
expect_error(check_all_recode(x, contains("not_there")),
             "no columns selected; check your variable name specification")
  }
)


vec <- c("Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree", "Frogs", NA)
vec_fac <- factor(vec, levels = vec)

test_that("recode default parameters are correct", {
  expect_equal(recode_to_binary(vec), factor(c("Selected", "Selected", rep("Not selected", 4), NA), levels = c("Selected", "Not selected", NA)))
})

test_that("recode produces intended result", {
  expect_equal(recode_to_binary(vec, label_matched = "Top-2", label_unmatched = "Not in Top-2"), factor(c("Top-2", "Top-2", rep("Not in Top-2", 4), NA), levels = c("Top-2", "Not in Top-2", NA)))
  expect_equal(recode_to_binary(vec, "frogs", label_matched = "Top-2", label_unmatched = "Not in Top-2"), factor(c(rep("Not in Top-2", 5), "Top-2", NA), levels = c("Top-2", "Not in Top-2", NA)))
  expect_equal(recode_to_binary(vec, c("unrelated term", "frogs"), label_matched = "Top-2", label_unmatched = "Not in Top-2"), factor(c(rep("Not in Top-2", 5), "Top-2", NA), levels = c("Top-2", "Not in Top-2", NA)))
})

test_that("same result on factor and character", {
  expect_equal(recode_to_binary(vec), recode_to_binary(vec_fac))
})

test_that("recode produces correct warning and result when nothing is found to recode", {
  expect_equal(suppressWarnings(
    recode_to_binary(vec, "not in the vector", label_matched = "Top-2", label_unmatched = "Not in Top-2")
    ),
    factor(c(rep("Not in Top-2", 6), NA), levels = c("Top-2", "Not in Top-2", NA))
  )
  expect_warning(recode_to_binary(vec, c("totally", "not in the vector")), "no instances of \"totally\", \"not in the vector\" found in x")
})

# TODO, if/when we use the label attribute to capture survey question text: check that column attributes are retained?



test_that("label attributes are skipped when set_labels = FALSE", {
  no_labels <- suppressWarnings(x %>% check_all_recode(contains("q1"), set_labels = FALSE))
  expect_equal(var_label(no_labels),
               list(unrelated = NULL, q1_1 = NULL, q1_2 = NULL, q1_3 = NULL, q1_4 = NULL, q1_other = NULL))

  expect_equal(no_labels %>% check_all_count(contains("q1")),
               data.frame(
                 response = c("q1_1", "q1_2", "q1_3", "q1_4", "q1_other"),
                 n = c(2, 2, 1, 0, 2),
                 percent = c(2/4, 2/4, 1/4, 0, 2/4),
                 stringsAsFactors = FALSE
               ) %>%
                 as_tabyl(1))
})


test_that("bad inputs error or warn appropriately", {
  expect_error(check_all_count(mtcars, cyl:carb),
               "input vectors should only have values of 0, 1, and NA; run check_all_recode() before calling this function", fixed = TRUE)
  expect_warning(mtcars %>% check_all_recode(cyl:carb),
                "column 1 has multiple values besides NA; not sure which is the question text.  Guessing this an \"Other (please specify)\" column.",
                fixed = TRUE)

})
