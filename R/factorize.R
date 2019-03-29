#' Calculate the percent of non-missing values in a character vector containing the values of interest.  This is a helper function for factorize_df().
#'
#' @param vec character vector.
#' @param valid_strings the values that the variable can possibly take on.
#'
#' @return numeric proportion between 0 and 1.
prop_matching <- function(vec, valid_strings){
  vec <- as.character(vec)
  if(all(is.na(vec))){
    message("Fully NA column detected; consider janitor::remove_empty()")
    return(0)
  }
  mean(vec[!is.na(vec)] %in% valid_strings)
}


#' @title Convert all character vectors containing a set of values in a data.frame to factors.
#' @description This function examines each column in a data.frame; when it finds a column composed solely of the values provided to the \code{lvls} argument it updates them to be factor variables, with levels in the order provided.
#'
#' This is an alternative to calling \code{dplyr::mutate_at} with \code{factor()} and identifying the specific variables you want to transform, if you have several repeated sets of responses.
#'
#' @param dat data.frame with some factor variables stored as characters.
#' @param lvls The factor levels in your variable(s), in order.  If you have a question whose possible responses are a subset of another question's, don't use this function; manipulate the specific columns with \code{dplyr::mutate_at}.
#'
#' @return data.frame with factorization completed in place.
#' @export
#' @examples
#' library(dplyr)
#' mtcars %>%
#'   dplyr::mutate(agr = rep(c("Somewhat agree", "Strongly disagree"), 16)) %>%
#'   factorize_df(lvls = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"))
#'
#' # prints warning due to case mismatches:
#' mtcars %>%
#'   dplyr::mutate(agr = rep(c("Somewhat Agree", "Strongly Disagree"), 16)) %>%
#'   factorize_df(lvls = c("Strongly disagree", "Somewhat disagree", "Somewhat agree", "Strongly agree"))
factorize_df <- function(dat, lvls){

  dat_out <- dat %>%
    dplyr::mutate_if( ~ prop_matching(.x, lvls) == 1, ~ factor(., lvls))


  # col types stored as list-columns to be robust to multi-part col classes, e.g., POSIX dates with two parts
  col_diffs <- cbind(lapply(dat, class),
                     lapply(dat_out, class)) %>%
    tibble::as_tibble(., rownames = "var_name") %>%
    dplyr::mutate(match_prop = purrr::map_dbl(dat, ~prop_matching(.x, lvls)))

  types_changed <- purrr::map2_lgl(col_diffs$V1, col_diffs$V2, purrr::negate(identical))

  changed_cols <- col_diffs$var_name[types_changed]

  # message if a match was _already_ a factor, the transformations made, or if no matches
  new_text <- ""
  if(any(col_diffs$match_prop == 1 & purrr::map_lgl(dat, inherits, "factor"))){
    warning("at least one matching column was already a factor, though this call will have reordered it if different levels provided.  Could be caused by overlapping sets of factor levels, e.g., \"Yes\", \"Maybe\", \"No\" and \"Yes\", \"No\".")
    new_text <- "new "
  }

  if(length(changed_cols) > 0) {
    message(paste("Transformed these columns: \n", paste("* ", changed_cols, collapse = ", \n")))
  } else{
    warning(paste0("No ", new_text, "columns matched.  Check spelling & capitalization of your levels."))
  }

  tibble::as_tibble(dat_out)

}
