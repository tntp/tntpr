# Tests for survey analysis functions

library(janitor)
library(dplyr)
context("survey utilities")


x <- data.frame( # 4th person didn't respond at all
  unrelated = 1:4, # not part of the question
  q1_1 = c("a", "a", NA, NA),
  q1_2 = c("b", "b", NA, NA),
  q1_3 = c(NA, NA, "c", NA),
  q1_4 = c(NA, NA, NA, NA) # no one selected this choice
)

library(dplyr) # for the %>% pipe
treated_x <- x %>%
  treat_check_all(q1_1:q1_4)

tabulated_x <- x %>%
  treat_check_all(q1_1:q1_4) %>%
  tabulate_check_all(q1_1:q1_4)

test_that("treatment performs as expected", {
  expect_equal(treated_x, data_frame(
    unrelated = 1:4,
    q1_1 = c("a", "a", "did not select", NA),
    q1_2 = c("b", "b", "did not select", NA),
    q1_3 = c("did not select", "did not select", "c", NA),
    q1_4 = c(rep("did not select", 3), NA)
  ))
})

test_that("tabulation performs as expected", {
  expect_equal(tabulated_x, data.frame(
    response = c("a", "b", "c", "q1_4"),
    n = c(2, 2, 1, 0),
    percent = c(2/3, 2/3, 1/3, 0),
    stringsAsFactors = FALSE
  ))
})


test_that("select helpers work", {
  expect_equal(x %>% treat_check_all(q1_1:q1_4) %>% tabulate_check_all(q1_1:q1_4),
               x %>% treat_check_all(contains("q1")) %>% tabulate_check_all(contains("q1")))
})

test_that("bad inputs are caught", {
expect_error(treat_check_all(contains("not_there")),
             "no columns selected; check your variable name specification")
  }
)


test_that("recode produces intended result", {
  vec <- c("Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Strongly disagree", "Frogs", NA)
  expect_equal(recode_top_2(vec), factor(c("Top-2", "Top-2", rep("Not in Top-2", 4), NA), levels = c("Top-2", "Not in Top-2", NA)))
  expect_equal(recode_top_2(vec, "frogs"), factor(c(rep("Not in Top-2", 5), "Top-2", NA), levels = c("Top-2", "Not in Top-2", NA)))
  expect_equal(recode_top_2(vec, c("unrelated term", "frogs")), factor(c(rep("Not in Top-2", 5), "Top-2", NA), levels = c("Top-2", "Not in Top-2", NA)))
})

test_that("recode produces correct warning and result when nothing is found to recode", {
  expect_equal(suppressWarnings(
    recode_top_2(vec, "not in the vector")
    ),
    factor(c(rep("Not in Top-2", 6), NA), levels = c("Top-2", "Not in Top-2", NA))
  )
  expect_warning(recode_top_2(vec, c("totally", "not in the vector")), "no instances of \"totally\", \"not in the vector\" found in x")
})
# TODO, if/when we use the label attribute to capture survey question text: check that column attributes are retained?
