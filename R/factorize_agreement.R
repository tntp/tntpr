#' Calculate the percent of non-missing values in a character vector containing the values of interest.  This is a helper function for factorize_df().
#'
#' @param vec character vector.
#' @param valid_strings the values that the variable can possibly take on.
#'
#' @return numeric proportion between 0 and 1.
prop_agreement <- function(vec, valid_strings){
  vec <- as.character(vec)
  if(all(is.na(vec))){
    message("Fully NA column detected; consider janitor::remove_empty()")
    return(0)
  }
  mean((vec %in% valid_strings), na.rm = TRUE)
}


#' Convert all character vectors containing a set of values in a data.frame to factors.
#'
#' @param dat data.frame with some factor variables stored as characters.
#' @param lvls The factor levels in your variable(s), in order.  If you have a question whose possible responses are a subset of another question's, don't use this function; manipulate the specific columns with \code{dplyr::mutate_at}.
#' @param cutoff (optional) what is the threshold for % of text values matching the agreement levels that causes factorization?  If unsure, leave at 1 - it should probably be 1 permanently with better error catching of values that don't fit the stated values paradigm.
#'
#' @return data.frame with factorization completed in place.
#' @export
#'
#' @examples
#' library(dplyr)
#' mtcars %>%
#'   dplyr::mutate(agr = rep(c("Somewhat Agree", "Strongly disagree"), 16)) %>%
#'   factorize_df(lvls = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"))
#'
#' # prints warning due to case mismatches:
#' mtcars %>%
#'   dplyr::mutate(agr = rep(c("Somewhat Agree", "Strongly Disagree"), 16)) %>%
#'   factorize_df(lvls = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"))
factorize_df <- function(dat, lvls, cutoff = 1){
  dat_out <- dat %>%
    dplyr::mutate_if( ~ prop_agreement(.x, lvls) >= cutoff, ~ factor(., lvls))

  col_diffs <- tibble::as_tibble(cbind(sapply(dat, class), sapply(dat_out, class)), rownames = NA) %>%
    tibble::rownames_to_column()
  changed_cols <- col_diffs$rowname[col_diffs[[2]] != col_diffs[[3]]]
  if(length(changed_cols) == 0){ warning("No columns matched  Check spelling & capitalization of your levels.")  }
  message(glue::glue("Transformed these columns: ", changed_cols))

  tibble::as_tibble(dat_out)

}
