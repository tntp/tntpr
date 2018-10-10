#' @title Add a cumulative sum column to a data.frame.
#'
#' @description A tidyverse-style function to add a cumsum column.
#'
#' @param dat data.frame to add cumulative sum to
#' @param colname specify the unquoted name of the column to sum, or leave it blank in which case the function will default first to a column called "valid_percent" and then for "percent".  These defaults support running this function on the result of calls to \code{janitior::tabyl}.  If no colname is supplied and these default columns are absent in the data.frame, the function will error.
#' @param dir direction to sum; defaults to "down" but can sum from the bottom of a data.frame with "up".  In this case the resulting column name will be "cumulative_up".
#'
#' @return a data.frame.
#' @export
#'
#' @examples
#' library(janitor)
#' mtcars %>%
#'   adorn_cumulative(mpg)
#'
#' mtcars %>%
#'   tabyl(cyl) %>%
#'   adorn_cumulative()
#'
#' # Vector with an NA
#' x <- c(0, 1, 2, 3, 3, 3, NA)
#'
#' x %>%
#'   tabyl() %>%
#'   adorn_cumulative()
#'
#' x %>%
#'   tabyl() %>%
#'   adorn_cumulative(dir = "up")

adorn_cumulative <- function(dat, colname, dir = "down"){

  if(!missing(colname)){
    colname <- rlang::enquo(colname)
  } else if("valid_percent" %in% names(dat)) {
  colname <- rlang::sym("valid_percent")
  } else if("percent" %in% names(dat)){
    colname <- rlang::sym("percent")
  } else {
    stop("\"colname\" not specified and default columns valid_percent and percent are not present in data.frame dat")
  }

  target <- dplyr::pull(dat, !! colname)

  if(dir == "up"){
    target <- rev(target)
  }
  dat$cumulative <- cumsum(ifelse(is.na(target), 0, target)) + target*0 # an na.rm version of cumsum, from https://stackoverflow.com/a/25576972
  if(dir == "up"){
    dat$cumulative <- rev(dat$cumulative)
    names(dat)[names(dat) %in% "cumulative"] <- "cumulative_up"
  }
  dat
}
