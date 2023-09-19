## ----setup,echo=FALSE, include = FALSE----------------------------------------
library(knitr)
knitr::opts_chunk$set(error = TRUE)
knitr::opts_chunk$set(out.width = "750px", dpi = 300)
knitr::opts_chunk$set(dev = "png", fig.width = 8, fig.height = 4.8889, dpi = 300)

## ----load_packages, include = FALSE-------------------------------------------
library(pacman)
if (!require("tntpr")) install_github("tntp/tntpr")
library(tntpr)
p_load(tidyverse, janitor, lubridate)

## ----echo=FALSE---------------------------------------------------------------
set.seed(1)

appl_dat <- tibble(
  student_id = 1:100,
  test_date = sample(seq(as.Date("2010/08/01"), as.Date("2020/01/01"), by = "day"), 100)
)

appl_dat

## -----------------------------------------------------------------------------
appl_dat %>%
  mutate(hire_date_sy = date_to_sy(test_date, last_day_of_sy = ymd("2018-06-01")))

