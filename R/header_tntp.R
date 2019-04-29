#' Insert header_script_tntp.
#'
#' Call this function to insert the header from 'TNTP R Shell.R'.
#'
#' @export
header_tntp <- function() {
  rstudioapi::insertText("# TITLE: [enter]
# AUTHOR(S): [enter]

# DESCRIPTION: [enter a few comments about what this script does]

# Load packages - first pacman, installing if necessary, then others
if (!require(\"pacman\")) install.packages(\"pacman\"); library(pacman)
p_load(tidyverse, janitor, readxl) # add more here as needed
if (!suppressPackageStartupMessages(require(\"tntpr\"))) {p_load(devtools); devtools::install_github(\"tntpr/tntpr\")}

# Load data

# Cleaning

# Analysis")
}
