#' @title Convert a date value into its school year.
#'
#' @md
#'
#' @description
#' Checks to see if a date is past the user-specified cutoff point for delineating school years, then maps to the appropriate year.
#'
#' @param date_var the date to convert. Can be a `Date` object or a string in the form 'YYYY-MM-DD' or 'MM/DD/YYYY'
#' @param last_day_of_sy the cutoff date, after which a date is considered part of the following school year. The year of this argument does not matter. Defaults (noisily) to July 1st.
#' @return Returns a character vector in the format of "2013 - 2014"

#' @examples
#' date_to_sy(as.Date("2014-05-05"), as.Date("2000-07-01"))
#' date_to_sy(as.Date("2014-07-05"), as.Date("2000-07-01"))
#'
#' @export

## Date to SY function
## Year of 2nd argument does not matter
## Turns 2015-10-02 into "2015-16", and 2016-04-05 into "2015-16", with cutoff day = 2010-07-01
date_to_sy <- function(date_var, last_day_of_sy = NULL) {
  if(is.null(last_day_of_sy)) {
    last_day_of_sy <- as.Date("2000-07-01")
    cli::cli_inform(c("!" = "No cutoff date provided. Using a default cutoff date of {.val {format(last_day_of_sy, '%B %d')}}"))
  }

  date_var <- parse_date(date_var)
  last_day_of_sy <- parse_date(last_day_of_sy)

  cutoff_day <- lubridate::day(last_day_of_sy)
  cutoff_month <- lubridate::month(last_day_of_sy)
  dplyr::case_when(
    is.na(date_var) ~ as.character(NA),
    lubridate::month(date_var) > cutoff_month ~ paste0(lubridate::year(date_var), " - ", lubridate::year(date_var) + 1), # if past cutoff, SY X - X+1
    lubridate::month(date_var) == cutoff_month & lubridate::day(date_var) > cutoff_day ~ paste0(lubridate::year(date_var), " - ", lubridate::year(date_var) + 1), # same month but greater day so past the cutoff, SY x - X +1
    TRUE ~ paste0(lubridate::year(date_var) - 1, " - ", lubridate::year(date_var)) # prior to cutoff = SY X-1 to X
  )
}

# Helper function. Returns a date object as is, or noisily attempts to parse
# a string in the form YYYY-MM-DD or MM/DD/YYYY
parse_date <- function(date) {

  parse_formats <- c("%Y-%m-%d",
                     "%m/%d/%Y")

  if(lubridate::is.Date(date)) {
    date
  } else {
    parsed_date <- tryCatch(
      as.Date(date, tryFormats = parse_formats),
      error = \(e) {
        cli::cli_abort(c("x" = "Could not parse date {.val {date}}",
                         "i" = "Pass a {.cls Date} object or a string in the form {.val YYYY-MM-DD} or {.val MM/DD/YYYY} instead"))
      })

    # Fix two-digit years (assumes 20..)
    yr <- lubridate::year(parsed_date)
    lubridate::year(parsed_date) <- yr + ifelse(yr < 100, 2000, 0)

    cli::cli_inform(c("i" = "Parsed {.val {date}} as {.val {format(parsed_date, '%B %d, %Y')}}"))
    parsed_date
  }
}
