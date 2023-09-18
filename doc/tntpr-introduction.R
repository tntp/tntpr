## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(tntpr)
library(tidyverse)

## ----include = FALSE----------------------------------------------------------
library(tntpr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)

## ----colors-------------------------------------------------------------------
palette_tntp("dark_blue", "orange", "light_gray")

## -----------------------------------------------------------------------------
palette_tntp_scales(palette = "likert_5pt")

## ----scale_fill_tntp, fig.width=7, fig.align='center', warning=FALSE----------
data.frame(
  question = "To what extent do you agree...",
  response = c(
    rep("Strongly disagree", 3),
    rep("Disagree", 4),
    rep("Somewhat disagree", 3),
    rep("Somewhat agree", 4),
    rep("Agree", 10),
    rep("Strongly agree", 2)
  )
) %>%
  mutate(response = response %>% factor(levels = rev(c(
    "Strongly disagree",
    "Disagree",
    "Somewhat disagree",
    "Somewhat agree",
    "Agree",
    "Strongly agree"
  )))) %>%
  ggplot(aes(question, fill = response)) +
  geom_bar(position = position_fill()) +
  theme_tntp_2018(axis_text = "Y", grid = FALSE) +
  labs(
    x = NULL, y = NULL,
    fill = "Response"
  ) +
  coord_flip() +
  scale_fill_tntp(palette = "likert_6pt")

