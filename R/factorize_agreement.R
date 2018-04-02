# Turns agreement vector into an ordered factor
# You can supply your own set of strings and it will use those levels

#' Title
#'
#' @param x the vector to factorize
#' @param lvls (optional) the levels, in order, of the resulting factor.  These should be provided as title case (see ?tools::toTitleCase).
#'
#' @return the vector, now as an ordered factor.  Currently the export will always return title case.
#' @export
#'
#' @examples
#'
#' x <- c("Strongly Agree", "Disagree")
#' janitor::tabyl(x) # not ordered correctly
#' y <- factorize_agreement(x)
#' janitor::tabyl(y) # ordered correctly, shows missing levels
factorize_agreement <- function(x, lvls){
  x <- tools::toTitleCase(x)
  x[x == "NA"] <- NA
  if(missing(lvls)){ lvls <- c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Somewhat Agree",
                              "Agree", "Strongly Agree") }
  x <- factor(x, levels = lvls)
  x
}

#' Calculate the percent of non-missing values in a character vector containing the term "agree."  This is a helper function for factorize_agreement_df().
#'
#' @param vec character vector.
#'
#' @return numeric proportion between 0 and 1.
prop_agreement <- function(vec){
  vec <- as.character(vec)
  if(all(is.na(vec))){
    message("Fully NA column detected; consider janitor::remove_empty()")
    return(0)
  }
  mean(stringr::str_detect(tolower(vec), "agree"), na.rm = TRUE)
}


#' Convert all agreement-seeming character vectors in a data.frame to factors, calling \code{factorize_agreement()} on each of those.
#'
#' @param dat data.frame with some agreement variables stored as characters.
#' @param lvls (optional) the agreement levels in your survey, in order.  If you have different agreement levels on the same survey, don't use this function, and use \code{factorize_agreement()} on the individual columns.  Default is a 6-pt scale from Strongly Disagree to Strongly Agree.
#' @param cutoff (optional) what is the threshold for % of text values matching the agreement levels that causes factorization?  If unsure, leave at 1 - it should probably be 1 permanently with better error catching of values that don't fit the stated values paradigm.
#'
#' @return data.frame with factorization completed in place.
#' @export
#'
#' @examples
#' library(dplyr)
#' mtcars %>%
#'   dplyr::mutate(agr = rep(c("Somewhat agree", "Strongly Disagree"), 16)) %>%
#'   factorize_agreement_df()
factorize_agreement_df <- function(dat, lvls, cutoff = 1){
  # TODO: Throw warning with values that don't fit the paradigm
  # TODO: Should these levels go the other way?
  if(missing(lvls)){ lvls <- c("Strongly Disagree", "Disagree", "Somewhat Disagree", "Somewhat Agree",
                               "Agree", "Strongly Agree") }

  dat %>%
    dplyr::mutate_if( ~ prop_agreement(.x) >= cutoff, ~ factorize_agreement(., lvls)) %>%
    tibble::as_tibble()
}
