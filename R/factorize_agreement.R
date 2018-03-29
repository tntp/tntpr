# Turns agreement vector into an ordered factor
# You can supply your own set of strings and it will use those levels

#' Title
#'
#' @param x the vector to factorize
#' @param lvls (optional) the levels, in order, of the resulting factor.  These shoudl be provided as title case (see ?tools::toTitleCase).
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
  if(missing(lvls)){ lvls <- c("Strongly Agree", "Agree", "Somewhat Agree", "Somewhat disagree", "Disagree", "Strongly Disagree") }
  x <- factor(x, levels = lvls)
  x
}

# Searches a data.frame for vectors where the six common agreement levels make up >50% of values
# Can supply own set of strings; calls factorize_agreement_vec
# Throw warning with values that don't fit the paradigm
# Returns data.frame with factorization completed in place
#  factorize_agreement_df <- function(x, lvls){

# }
