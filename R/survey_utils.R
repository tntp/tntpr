#' @title Recode a variable into "Top-2" or "Not in Top-2".
#'
#' @description
#' Recodes a character variable into a binary result: "Top-2" if the value matches any of the supplied character strings in the \code{top_2_values} vector, "Not in Top-2" if not.  NA remains NA.
#'
#' This recoding is not case-sensitive; if you specify "agree" as a top-2 value, "Agree" will be counted as Top-2, and vice versa.
#'
#' You don't *have* to specify two top levels; you can specify one, three, bottom levels, etc.
#'#'
#' @param x the character or factor vector to be recoded
#' @param top_2_values a character vector with the strings that should be considered "Top-2".  Defaults to "strongly agree" and "agree" but can be overwritten.
#' @return a factor variable (for nicer ordering in calls to \code{janitor::tabyl}) with values mapped to Top-2 and Not in Top-2.
#' @export
#' @examples
#' agreement <- c("Strongly agree", "Agree", "Somewhat agree",
#'   "Somewhat disagree", "Strongly disagree", "Frogs", NA)
#' recode_top_2(agreement) # default values of "strongly agree" and "agree" are used for recoding
#' recode_top_2(agreement, "frogs")
#'

#' freq <- c("always", "often", "sometimes", "never")
#' recode_top_2(freq, "always")

recode_top_2 <- function(x, top_2_values = c("strongly agree", "agree")){

  # QC: Throw an error if top_2_values is not character vector
  testthat::expect(exp = is.character(top_2_values),
                   "You must supply a character vector to the top_2_values argument")

  x           <- tolower(x)
  top2_values <- tolower(top_2_values)

  if(sum(x %in% top2_values, na.rm = TRUE) == 0){ warning(paste0("no instances of ", paste(paste0("\"", top_2_values, "\""), collapse = ", "), " found in x")) }

  result <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% top2_values ~ "Top-2",
    ! x %in% top2_values ~ "Not in Top-2"
    )

  factor(result, levels = c("Top-2", "Not in Top-2", NA))
}

#' @title Process a range of check-all-that-apply response columns for correct tabulation.
#'
#' @description
#' Some survey software returns check-all-that-apply response columns where missing values could indicate either that the respondent skipped the question entirely, or that they did not select that particular answer choice.  To count the responses properly, the cases where a respondent did not check any of choices - i.e., they skipped the question - should not be counted in the denominator (assuming that the choices were completely exhaustive, or that there was an NA option).
#'
#' This function takes a data.frame and range of columns containing all answer choices to a check-all-that-apply question and updates the columns in the data.frame to contain one of three values: 1 if the choice was selected; 0 if the respondent chose another option but not this one; or NA if the respondent skipped the question (i.e., they did not select any of the choices) and thus their response is truly missing.
#'
#' It also takes the single text values in each column and adds them as a \code{label} attribute to each data.frame columns.
#'
#' \code{check_all_recode()} prepares the data.frame for a call to its sister function \code{check_all_count()}.  The label attribute is accessed by this function.
#'
#' @param dat a data.frame with survey data
#' @param ... unquoted variable names containing the answer choices.  Can be specified as a range, i.e., \code{q1_1:q1_5} or using other helper functions from \code{dplyr::select()}.
#' @param set_labels should the label attribute of the columns be over-written with the column text?  Allow this to be TRUE unless there are currently label attributes you don't wish to overwrite.
#' @return Returns the original data.frame with the specified column range updated, and with label attributes on the questions.
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
#'   check_all_recode(q1_1:q1_3)
#'
#' # You can use any of the dplyr::select() helpers to identify the columns:
#' x %>%
#'   check_all_recode(contains("q1"))
#'
check_all_recode <- function(dat, ..., set_labels = TRUE){
  dat <- dplyr::as_data_frame(dat) # so that single bracket subsetting behaves as expected later and returns a data.frame
  original_order <- names(dat)
  cols_of_interest <- dat %>% dplyr::select(...) %>% as.data.frame()
  if(ncol(cols_of_interest) == 0){ stop("no columns selected; check your variable name specification") }
  responded <- (rowSums(!is.na(cols_of_interest)) > 0)

  # set label attribute
  labels_of_interest <- check_all_q_text_to_label(cols_of_interest)

  # convert factors to characters so that values can be updated
  fac_index <- unlist(lapply(cols_of_interest, is.factor))
  cols_of_interest[fac_index] <- lapply(cols_of_interest[fac_index], as.character)

  # update columns of interest to be NA if responded == FALSE, otherwise turned checked/blank into 1/0
  cols_of_interest[!responded, ] <- NA_character_
  cols_of_interest[responded, ][!is.na(cols_of_interest[responded, ])] <- 1
  cols_of_interest[responded, ][is.na(cols_of_interest[responded, ])] <- 0
  # convert columns to numeric
  cols_of_interest <- lapply(cols_of_interest, as.numeric) %>% dplyr::as_data_frame()

  # restore labels
  if(set_labels){ labelled::var_label(cols_of_interest) <- labels_of_interest }

  # rejoin back to main df, reorder
  dat <- cbind(
    dat[, setdiff(names(dat), names(cols_of_interest))], # dat needs to be a tibble or else a single col not part of the check-all range will come back as a vector
    cols_of_interest
  )

  dat[, original_order] # reorder to original col order

}

#' @title Tabulate a range of check-all-that-apply response columns in a single table.
#'
#' @description
#' This function is to be run on columns treated with \code{check_all_recode()}.
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
#'   check_all_recode(q1_1:q1_3) %>%
#'   check_all_count(q1_1:q1_3)
#'
#' # You can use any of the dplyr::select() helpers to identify the columns:
#' x %>%
#'   check_all_recode(contains("q1")) %>%
#'   check_all_count(contains("q1"))


# Conveniently the same format as a janitor::tabyl() so can use its helpers when they are developed
check_all_count <- function(dat, ...){
  if(nrow(dat) == 0){stop("input data.frame \"dat\" has zero rows")}
  if(sum(dat == 0, na.rm = TRUE) == 0){warning("there are no values of \"0\" in these columns.  Either you need to first call check_all_recode(), or every respondent selected every possible answer.")}

  cols_of_interest <- dat %>% dplyr::select(...) %>% as.data.frame()
  result <- dplyr::bind_rows(
    lapply(
      X = cols_of_interest, FUN = count_single_col
    )
  )

  # set first column of result (option name). Ideally this is stored in a label attribute.  This checks for that presence, if it doesn't exist, it uses the variable name.
  var_labels <- unlist(labelled::var_label(dat))
  ifelse(is.null(var_labels), # if no variable labels were set for some reason, say set_labels = FALSE
         result$response <- names(cols_of_interest),
         result$response <- ifelse(is.na(var_labels), names(cols_of_interest), var_labels)
  )


  result
}

# Helper function that returns a single row data.frame for a check-all-that-apply question
# Assumes column is first treated with check_all_recode()
count_single_col <- function(vec){
  var_name <- deparse(substitute(vec))

  if(!is.numeric(vec)){ stop("column is not of type numeric, run check_all_recode() before calling this function") }
  if(sum(! vec %in% c(0, 1, NA)) > 0){ stop("input vectors should only have values of 0, 1, and NA; run check_all_recode() before calling this function")}

  actuals <- vec[!is.na(vec)]
  n_selected <- sum(actuals)
  perc <- n_selected / length(actuals)
  data.frame(
    response = var_name,
    n = n_selected,
    percent = perc,
    stringsAsFactors = FALSE
  )
}

# function called by check_all_recode; grabs question text and adds it as column attributes, for use in check_all_count.  Returns list of variable labels.
check_all_q_text_to_label <- function(dat){
  for(i in seq_along(dat)){
    if(!is.null(unlist(labelled::var_label(dat[i])))){ warning("column already has a label attribute, overwriting with check-all option text")}
    q_text <- unique(dat[[i]][!is.na(dat[[i]])])
    if(length(q_text) > 1){ stop("column has multiple text values besides NA; not sure which is the question text.  Is this a check-all-that-apply column?") }
    if(length(q_text) == 0){ q_text <- NA_character_ } # in case a column was all NAs
    var_label(dat[i]) <- as.character(q_text)
  }
  labelled::var_label(dat)
}
