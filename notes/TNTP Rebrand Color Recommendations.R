# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, scales) # add more here as needed
pacman::p_load_current_gh("tntp/tntpr")

#
scales::show_col(c("#BC5A07", "#EA8835", "#FFB562", "#81D2EE", "#00A4c7", "#00355F"), ncol = 6)
scales::show_col(c("#BC5A07", "#EA8835", "#c1c2c4", "#00A4c7", "#00355F"), ncol = 5)
scales::show_col(c("#BC5A07", "#EA8835", "#00A4c7", "#00355F"), ncol = 4)
