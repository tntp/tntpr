recode_top_2 <- function(x, top_2_values = c("strongly agree", "agree")){

  # QC: Throw an error if x is not specified  ---------------------------------
  testthat::expect(exp = !missing(x),
                   "Supplying a vector to the x argument is required")

  # QC: Throw an error if top_2_values is not character vector ----------------
  testthat::expect(exp = is.character(top_2_values),
                   "You must supply a character vector to the top_2_values argument")

  x           <- tolower(x)
  top2_values <- tolower(top_2_values)

  dplyr::if_else(condition = is.na(x),
                 true      = NA_character_,
                 false     = if_else(condition = x %in% top2_values,
                                     true      = "Top-2 Agree",
                                     false     =  "Not in Top-2"))
}

#' @title Process a range of check-all-that-apply response columns for correct tabulation.
#'
#' @description
#' Some survey software returns check-all-that-apply response columns where missing values could indicate either that the respondent skipped the question entirely, or that they did not select that particular answer choice.  To count the responses properly, the cases where a respondent did not check any choices should not be counted in the denominator (assuming that the choices were MECE, or that there was an NA option).
#'
#' This function takes a data.frame and range of columns containing all answer choices to a check-all-that-apply question and updates the columns in the data.frame to contain one of three values: the original value if the choice was selected; "did not select" if the respondent chose another option but not this one; or "did not answer question" if the respondent skipped the question (i.e., they did not select any of the choices).
#'
#' @param dat a data.frame with survey data
#' @param ... unquoted variable names containing the answer choices.  Can be specified as a range, i.e., \code{q1_1:q1_5} or using other helper functions from \code{dplyr::select()}.
#' @return Returns the data.frame with the selected column range updated.
#' @export
#' @examples
#' x <- data.frame( # 4th person didn't respond at all
#'   unrelated = 1:4,
#'   q1_1 = c("a", "a", "a", NA),
#'   q1_2 = c("b", "b", NA, NA),
#'   q1_3 = c(NA, NA, "c", NA)
#' )
#' library(dplyr) # for the %>% pipe
#' x %>%
#'   treat_check_alls(q1_1:q1_3)
#'
# returns the data frame with the treated col range mutated to be the original value, "did not select" or "did not answer question"
treat_check_alls <- function(dat, ...){
  ### TODO: error if the columns specified aren't in the data.frame.  See get_dupes() from janitor?
  dat <- as_data_frame(dat) # so that single bracket subsetting behaves as expected later and returns a data.frame
  original_order <- names(dat)



  cols_of_interest <- dat %>% select(...) %>% as.data.frame()
  responded <- (rowSums(!is.na(cols_of_interest)) > 0)

  # convert factors to characters so that values can be updated
  fac_index <- unlist(lapply(cols_of_interest, is.factor))
  cols_of_interest[fac_index] <- lapply(cols_of_interest[fac_index], as.character)

  # update columns of interest to be NA if responded == FALSE, otherwise checked
  # with numerics, can easily average them, etc. - but is character clearer?
  cols_of_interest[!responded, ] <- "did not answer question"
  # cols_of_interest[responded, ][!is.na(cols_of_interest[responded, ])] <- 1
  cols_of_interest[responded, ][is.na(cols_of_interest[responded, ])] <- "did not select"


  # rejoin back to main df, reorder
  dat <- bind_cols(
    dat[, setdiff(names(dat), names(cols_of_interest))], # dat needs to be a tibble or else a single col not part of the check-all range will come back as a vector
    cols_of_interest
  )

  dat[, original_order] # reorder to original col order

}

#' @title Tabulate a range of check-all-that-apply response columns in a single table.
#'
#' @description
#' This function is to be run on columns treated with \code{treat_check_alls()}.
#'
#' Takes a data.frame and range of columns containing all answer choices to a check-all-that-apply question and tabulates the results.  People who did not select any choices  (i.e., they did not answer the question) are omitted from the denominator.  For this to make sense, the question's choices should be MECE, or there should be an NA option.
#'
#' @param dat a data.frame with survey data
#' @param ... unquoted variable names containing the answer choices.  Can be specified as a range, i.e., \code{q1_1:q1_5} or using other helper functions from \code{dplyr::select()}.
#' @return Returns a data.frame with the tabulated results (n and % of question respondents choosing each choice.)
#' @export
#' @examples
#' x <- data.frame( # 4th person didn't respond at all
#'   unrelated = 1:4,
#'   q1_1 = c("a", "a", "a", NA),
#'   q1_2 = c("b", "b", NA, NA),
#'   q1_3 = c(NA, NA, "c", NA)
#' )
#' library(dplyr) # for the %>% pipe
#' x %>%
#'   treat_check_alls(q1_1:q1_3) %>%
#'   tabulate_check_all(q1_1:q1_3)
#'

# Conveniently the same format as a janitor::tabyl() so can use its helpers when they are developed
tabulate_check_all <- function(dat, ...){

  cols_of_interest <- dat %>% select(...) %>% as.data.frame()
  responded <- (rowSums(!is.na(cols_of_interest)) > 0)

  bind_rows(
    lapply(
      cols_of_interest, count_single_col
    )
  )

}

# Helper function that returns a single row data.frame for a check-all-that-apply question
# Assumes column is first treated with treat_check_all()
count_single_col <- function(vec){
  if(sum(is.na(vec)) > 0){ stop("there should not be NAs in any of these columns; run treat_check_all() before calling this function") }
  text <- unique(vec[! vec %in% c("did not answer question", "did not select")])
  if(length(text) > 1){ stop("there must only be one value besides \"did not answer question\" and \"did not select\"; run treat_check_all() before calling this function")}
  actuals <- vec[vec != "did not answer question"]
  n_selected <- sum(actuals != "did not select")
  perc <- n_selected / length(actuals)
  data.frame(
    response = text,
    n = n_selected,
    percent = perc,
    stringsAsFactors = FALSE
  )
}

