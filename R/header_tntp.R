#' Insert header_script_tntp.
#'
#' Call this function from inside a .R file in RStudio to insert the standard
#' TNTP header into your active script.
#'
#' @export
#' @returns nothing
#' @examplesIf rstudioapi::isAvailable()
#' header_tntp()
#'
header_tntp <- function() {
  rstudioapi::insertText("# TITLE: [enter]
# AUTHOR(S): [enter]
# DATE: `r Sys.Date()`

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require(\"pacman\")) install.packages(\"pacman\"); library(pacman)
pacman::p_load(here, readxl, tidyverse, janitor) # add more here as needed
pacman::p_load_gh(\"tntp/tntpr\", \"adamMaier/reviewr\", \"adamMaier/tntpmetrics\")

# Load data

# Cleaning

# Analysis")
}
