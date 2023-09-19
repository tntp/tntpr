## ----setup,echo=FALSE, include = FALSE----------------------------------------
library(knitr)
knitr::opts_chunk$set(error = TRUE)
knitr::opts_chunk$set(out.width = "750px", dpi = 300)
knitr::opts_chunk$set(dev = "png", fig.width = 8, fig.height = 4.8889, dpi = 300)
knitr::opts_chunk$set(fig.path = "introduction_files/")

## ----load_packages, include = FALSE-------------------------------------------
library(pacman)
if (!require("tntpr")) install_github("tntp/tntpr")
library(tntpr)
p_load(tidyverse, janitor, praise)

## ----example_data, echo=FALSE, include=FALSE----------------------------------
qtype_1 <- c("Strongly Disagree", "Disagree", "Somewhat disagree", "Somewhat agree", "Agree", "Strongly agree")
qtype_2 <- c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree")
qtype_3 <- c("No", "Yes")

survey_dat <- tibble(
  response_id = 1:100,
  years_of_experience = round(runif(100) * 10, digits = 0),
  q1 = sample(qtype_1, 100, replace = TRUE),
  q2 = sample(qtype_1 %>% str_remove("Strongly disagree"), 100, replace = TRUE),
  q3 = sample(qtype_1, 100, replace = TRUE),
  q4 = replicate(100, praise()),
  q5 = sample(qtype_2, 100, replace = TRUE),
  q6 = sample(qtype_2, 100, replace = TRUE),
  q7 = replicate(100, praise("${Exclamation}! ${EXCLAMATION}!-${EXCLAMATION}! This is just ${adjective}!")),
  q8 = sample(qtype_3, 100, replace = TRUE),
  q9 = sample(qtype_3, 100, replace = TRUE)
)

## -----------------------------------------------------------------------------
survey_dat %>%
  glimpse()

## -----------------------------------------------------------------------------
survey_dat %>%
  map(unique) %>%
  map(length)

## -----------------------------------------------------------------------------
survey_dat %>%
  select(q1, q2, q3, q5, q6, q8, q9) %>%
  map(unique)

## -----------------------------------------------------------------------------
survey_dat$q1 %>% unique()

## -----------------------------------------------------------------------------
survey_dat <- survey_dat %>%
  factorize_df(lvls = c("Strongly Disagree", "Disagree", "Somewhat disagree", "Somewhat agree", "Agree", "Strongly agree"))

survey_dat %>%
  glimpse()

survey_dat %>%
  map(levels)

## -----------------------------------------------------------------------------
survey_dat$q5 %>% unique()

## -----------------------------------------------------------------------------
survey_dat <- survey_dat %>%
  factorize_df(lvls = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))

## -----------------------------------------------------------------------------
survey_dat$q8 %>% unique()

## -----------------------------------------------------------------------------
survey_dat <- survey_dat %>%
  factorize_df(lvls = c("No", "Yes"))

## -----------------------------------------------------------------------------
survey_dat %>%
  select(q1, q2, q3, q5, q6, q8, q9) %>%
  map(is.factor)

## -----------------------------------------------------------------------------
survey_dat %>%
  select(q1, q2, q3, q5, q6, q8, q9) %>%
  map(levels)

