#' Insert header_script_tntp.
#'
#' Call this function to insert the header from 'TNTP R Shell.R'.
#'
#' @export
header_tntp <- function() {
  rstudioapi::insertText("# TITLE: [enter]
# AUTHOR(S): [enter]
# DATE: `r Sys.Date()`

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require(\"pacman\")) install.packages(\"pacman\"); library(pacman)
pacman::p_load(here, readxl, tidyverse, janitor) # add more here as needed
pacman::p_laod_gh(tntp/tntpr, amaier/reviewr, adamMaier/tntpmetrics)

# Load data

# Cleaning

# Analysis")
}
