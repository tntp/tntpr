#' @title Recode a variable into binary groups, e.g., "Top-2" and "Not in Top-2".
#'
#' @description
#' Recodes a character variable into a binary result, a two-level factor.  All values matching of the supplied character strings in the \code{to_match} vector are coded into the first level of the factor; all other values are coded into the other level.  \code{NA} remains \code{NA}.  The default factor labels are "Selected"  and "Not selected" but these can be overridden.
#'
#' This recoding is not case-sensitive; if you specify "agree" as a top-2 value, "Agree" will be counted as Top-2, and vice versa.
#'
#' @param x the character or factor vector to be recoded
#' @param to_match a character vector with the strings that should be put in the first level of the factor.  Defaults to "strongly agree" and "agree" but can be overwritten.
#' @param label_matched what should be the factor label of values that match the strings specified in \code{to_match}?  Defaults to "Selected"
#' @param label_unmatched what should be the factor label of values that don't match the strings specified in \code{to_match}?  Defaults to "Not selected".
#' @return a factor variable (for nicer ordering in calls to \code{janitor::tabyl}) with values mapped to the two levels.
#' @export
#' @examples
#' agreement <- c(
#'   "Strongly agree", "Agree", "Somewhat agree",
#'   "Somewhat disagree", "Strongly disagree", "Frogs", NA
#' )
#'
#' recode_to_binary(agreement) # default values of "strongly agree" and "agree" are used for recoding
#' recode_to_binary(agreement,
#'   label_matched = "Top-2 Agree",
#'   label_unmatched = "Not in Top-2"
#' ) # custom labels of factor levels
#' recode_to_binary(agreement, "frogs")
#' recode_to_binary(
#'   agreement,
#'   "frogs",
#'   "FROGS!!!",
#'   "not frogs"
#' ) # custom matching values & labels of factor levels
#'

#' freq <- c("always", "often", "sometimes", "never")
#' recode_to_binary(freq, "always", "always", "less than always")
recode_to_binary <- function(x, to_match = c("strongly agree", "agree"), label_matched = "Selected", label_unmatched = "Not selected") {
  if (!is.character(to_match)) {
    stop("You must supply a character or factor vector to the to_match argument")
  }

  x <- tolower(x)
  top2_values <- tolower(to_match)

  if (sum(x %in% top2_values, na.rm = TRUE) == 0) {
    warning(paste0("no instances of ", paste(paste0("\"", to_match, "\""), collapse = ", "), " found in x"))
  }

  result <- dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% top2_values ~ label_matched,
    !x %in% top2_values ~ label_unmatched
  )

  factor(result, levels = c(label_matched, label_unmatched, NA))
}

# How does that ^^^^ function compare to this stub from elsewhere?
convert_to_top_2_agree <- function(x, custom_vals = NULL) {
  if (is.null(custom_vals)) {
    custom_vals <- c(
      "strongly agree", "agree", "highly satisfied",
      "extremely satisfied", "satisfied", "very confident",
      "confident", "all", "most", "yes"
    )
    x <- tolower(x)
  }
  if_else(is.na(x), as.character(NA),
    if_else(x %in% custom_vals, "Top-2 Agree", "Not in Top-2")
  )
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
#' This function accomodates an open-response column, to get the correct denominator when some respondents have skipped all check variables but written something in.  This passing over of the offered choices is an implicit rejection of them, not a "missing."  Such a text variable will throw a warning - which may be okay - and will then be recoded into a binary 1/0 variable indicating a response.  Such a text variable will be assigned the label "Other".  Consider preserving the original respondent text values prior to this point as a separate column if needed.
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
#'   unrelated = 1:5,
#'   q1_1 = c("a", "a", "a", NA, NA),
#'   q1_2 = c("b", "b", NA, NA, NA),
#'   q1_3 = c(NA, NA, "c", NA, NA),
#'   q1_other = c(NA, "something else", NA, NA, "not any of these")
#' )
#' library(dplyr) # for the %>% pipe
#' x %>%
#'   check_all_recode(q1_1:q1_other)
#'
#' # You can use any of the dplyr::select() helpers to identify the columns:
#' x %>%
#'   check_all_recode(contains("q1"))
#'
check_all_recode <- function(dat, ..., set_labels = TRUE) {
  dat <- dplyr::as_data_frame(dat) # so that single bracket subsetting behaves as expected later and returns a data.frame
  original_order <- names(dat)
  cols_of_interest <- dat %>%
    dplyr::select(...) %>%
    as.data.frame()
  if (ncol(cols_of_interest) == 0) {
    stop("no columns selected; check your variable name specification")
  }
  responded <- (rowSums(!is.na(cols_of_interest)) > 0)

  # capture label attribute - will need to assign them later, so they aren't lost during the interim lines of code
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
  if (set_labels) {
    labelled::var_label(cols_of_interest) <- labels_of_interest
  }

  # rejoin modified columns back to main df
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
#' This works with an "Other" open-response text field, which will be recoded to a binary variable with \code{check_all_recode}.
#'
#' @param dat a data.frame with survey data
#' @param ... unquoted column names containing the range of the answer choices.  Can be specified individually, as a range, i.e., \code{q1_1:q1_5}, or using other helper functions from \code{dplyr::select()}.
#' @return Returns a data.frame with the tabulated results (n and % of question respondents choosing each choice.)  This is an object of class \code{tabyl} which means that you can call \code{janitor::adorn_pct_formatting()} and it will format the correct columns.
#' @export
#' @examples
#' x <- data.frame( # 4th person didn't respond at all
#'   unrelated = 1:5,
#'   q1_1 = c("a", "a", "a", NA, NA),
#'   q1_2 = c("b", "b", NA, NA, NA),
#'   q1_3 = c(NA, NA, "c", NA, NA),
#'   q1_other = c(NA, "something else", NA, NA, "not any of these")
#' )
#' library(dplyr) # for the %>% pipe
#' x %>%
#'   check_all_recode(q1_1:q1_other) %>%
#'   check_all_count(q1_1:q1_other)
#'
#' # You can use any of the dplyr::select() helpers to identify the columns:
#' x %>%
#'   check_all_recode(contains("q1")) %>%
#'   check_all_count(contains("q1"))
# Returns a janitor::tabyl object so can use the adorn_ functions
check_all_count <- function(dat, ...) {
  if (nrow(dat) == 0) {
    stop("input data.frame \"dat\" has zero rows")
  }
  if (sum(dat == 0, na.rm = TRUE) == 0) {
    warning("there are no values of \"0\" in these columns.  Either you need to first call check_all_recode(), or every respondent selected every possible answer.")
  }

  cols_of_interest <- dat %>%
    dplyr::select(...) %>%
    as.data.frame()
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


  result %>%
    janitor::as_tabyl(1) # set attributes to be a one-way tabyl as then adorn_pct_formatting works
}

# Helper function that returns a single row data.frame for a check-all-that-apply question
# Assumes column is first treated with check_all_recode()
count_single_col <- function(vec) {
  var_name <- deparse(substitute(vec))

  if (!is.numeric(vec)) {
    stop("column is not of type numeric, run check_all_recode() before calling this function")
  }
  if (sum(!vec %in% c(0, 1, NA)) > 0) {
    stop("input vectors should only have values of 0, 1, and NA; run check_all_recode() before calling this function")
  }

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
check_all_q_text_to_label <- function(dat) {
  for (i in seq_along(dat)) {
    if (!is.null(unlist(labelled::var_label(dat[i])))) {
      warning("column already has a label attribute, overwriting with check-all option text")
    }
    q_text <- unique(dat[[i]][!is.na(dat[[i]])])
    if (length(q_text) > 1) {
      warning(paste("column", i, "has multiple values besides NA; not sure which is the question text.  Guessing this an \"Other (please specify)\" column.", sep = " "))
      q_text <- "Other"
    }
    if (length(q_text) == 0) {
      q_text <- NA_character_
    } # in case a column was all NAs
    labelled::var_label(dat[i]) <- as.character(q_text)
  }
  labelled::var_label(dat)
}
