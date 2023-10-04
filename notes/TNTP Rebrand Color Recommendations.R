# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, scales) # add more here as needed
pacman::p_load_current_gh("tntp/tntpr")

#
scales::show_col(c("#BC5A07", "#EA8835", "#FFB562", "#81D2EE", "#00A4c7", "#00355F"), ncol = 6)
scales::show_col(c("#BC5A07", "#EA8835", "#c1c2c4", "#00A4c7", "#00355F"), ncol = 5)
scales::show_col(c("#BC5A07", "#EA8835", "#00A4c7", "#00355F"), ncol = 4)

# New (2023) colors

# Primary
black       <- '#000000'
light_gray  <- '#F1F1EE'
light_green <- '#E2EDDC'
white       <- '#FFFFFF'

# Secondary
yellow      <- '#FDE57B'
light_red   <- '#FDDACA'
medium_blue <- '#00A5C7'
dark_red    <- '#C31F46'
dark_green  <- '#317D5C'

# Extended
dark_blue   <- '#00355F'
orange      <- '#F26C4C'
dark_gray   <- '#4A4A4A'
dark_yellow <- '#F2CF13'
light_blue  <- '#81D3EB'
medium_red  <- '#DA8988'
medium_green <- '#8FB09D'

# 4pt likert
scales::show_col(c(dark_red, light_red, light_green, dark_green), ncol = 4)

# 5pt likert
scales::show_col(c(dark_red, light_red, light_gray, light_green, dark_green), ncol = 5)

# 6pt likert
scales::show_col(c(dark_red, medium_red, light_red, light_green, medium_green, dark_green), ncol = 6)
