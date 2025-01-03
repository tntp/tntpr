
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/tntp/tntpr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tntp/tntpr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<a href="https://tntp.org" target="_blank">
<img src="man/figures/logo.png" width="70" height="70"
alt="TNTP logo" /> </a>

# tntpr

## About

The `tntpr` package contains an assortment of functions and templates
customized to meet the needs of data analysts at the non-profit
organization TNTP. It includes functions for branded colors and plots,
credentials management, repository set-up, and other common analytic
tasks.

## Package summary

Some of the highlights of the package include:

- TNTP brand colors and palettes with the `tntp_colors()` and
  `tntp_palette()` functions
- A TNTP ggplot2 theme using brand fonts (`tntp_style()`)
- The `tntp_cred()` functions for securely managing credentials
  (passwords, API keys, etc.).
- The `sp_*()` functions for interacting with Sharepoint through R.
- TNTP-themed RMarkdown templates, for starting a new analysis with a
  shell that can already generate a TNTP-themed .docx report
- Functions for initializing a new repository or project folder with
  TNTP-standard directories and documentation
- Survey analysis tools such as `factorize_df()`, `recode_to_binary()`,
  and functions for dealing with check-all style questions
- Wrappers for quickly making typical TNTP-style charts (e.g., bar chart
  of means on variable 1 grouped by variable 2)
- Education-specific data management functions (e.g., `date_to_sy()` to
  convert continuous hire dates into school years using a specified
  cutoff date)
- Built-in fake data sets to practice with, including student
  achievement data (`wisc`), teacher data (`fake_county`) and survey
  data (`teacher_survey`)

`tntpr` is built to work with the tidyverse set of packages.

## Installing the `tntpr` package

You can install `tntpr` from CRAN as follows:

``` r
install.packages('tntpr')
```

You can also install this package from its GitHub repository using
`devtools`. If you do not have the `devtools` package installed, you
will have to run the first line in the code below as well:

``` r
# install.packages('devtools')
devtools::install_github('tntp/tntpr')
```

Once installed, you load it like any other package.

``` r
library(tntpr)
```

## Feature Requests and Bug Reports

Have a data problem you think `tntpr` could help with? Find a bug while
working with the `tntpr` package? Please create an issue.
