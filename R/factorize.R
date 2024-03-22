#' Calculate the percent of non-missing values in a character vector containing the values of interest.  This is a helper function for factorize_df().
#'
#' @param vec character vector.
#' @param valid_strings the values that the variable can possibly take on.
#' @param ignore.case if TRUE, ignores case in matching
#'
#' @returns a numeric proportion between 0 and 1.
prop_matching <- function(vec, valid_strings, ignore.case = FALSE) {
  vec <- as.character(vec)
  if (all(is.na(vec))) {
    message("Fully NA column detected; consider janitor::remove_empty()")
    return(0)
  }
  if(ignore.case) {
    vec <- tolower(vec)
    valid_strings <- tolower(valid_strings)
  }
  mean(vec[!is.na(vec)] %in% valid_strings)
}

#' Update case of a character vector
#'
#' Helper function for factorize_df(). Returns a vector of the same length as vec,
#' with any values that match values in valid_strings updated to the case in valid_strings
#'
#' @param vec The character vector you want to update
#' @param new_case A character vector of correctly cased strings
#'
#' @returns a character vector the same length as vec
#'
standardize_case <- function(vec, new_case) {

  names(new_case) <- tolower(new_case)
  vec_l <- tolower(vec)

  ifelse(vec_l %in% names(new_case),
         new_case[vec_l], vec)
}


#' @title Convert all character vectors containing a set of values in a data.frame to factors.
#' @description This function examines each column in a data.frame; when it finds a column composed solely of the values provided to the \code{lvls} argument it updates them to be factor variables, with levels in the order provided.
#'
#' This is an alternative to calling \code{dplyr::mutate_at} with \code{factor()} and identifying the specific variables you want to transform, if you have several repeated sets of responses.
#'
#' @md
#' @param dat data.frame with some factor variables stored as characters.
#' @param lvls The factor levels in your variable(s), in order.  If you have a question whose possible responses are a subset of another question's, don't use this function; manipulate the specific columns with \code{dplyr::mutate_at}.
#' @param ignore.case Logical. If TRUE, will match without checking case, using the capitalization from the `lvls` parameter for the final output. If not provided, the function will provide a warning if it detects columns that would match without checking case but will NOT coerce them.
#'
#' @returns a data.frame the same size as `dat`, with factorization completed in place.
#' @export
#' @examples
#' teacher_survey |>
#'   factorize_df(lvls = c("Strongly Disagree", "Disagree", "Somewhat Disagree",
#'                         "Somewhat Agree", "Agree", "Strongly Agree"))
#'
#' # prints warning due to case mismatches:
#' teacher_survey |>
#'   factorize_df(lvls = c("Strongly disagree", "Disagree", "Somewhat disagree",
#'                         "Somewhat agree", "Agree", "Strongly agree"))
factorize_df <- function(dat, lvls, ignore.case = NULL) {

  match_exact <- sapply(dat, prop_matching, lvls)
  match_all <- sapply(dat, prop_matching, lvls, ignore.case=TRUE)

  # Do any columns match without case, but not match exactly?
  match_nocase <- (match_all == 1) & (match_exact < 1)

  dat_out <- dat

  if(is.null(ignore.case) || !ignore.case) {
    transform_cols <- names(match_exact[match_exact == 1])

  } else {
    transform_cols <- names(match_all[match_all == 1])
    # Update capitalization if ignoring case
    dat_out <- dat_out |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(transform_cols),
                                  ~standardize_case(., lvls)))
  }

  dat_out <- dat_out |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(transform_cols),
                                ~factor(., lvls)))

  # col types stored as list-columns to be robust to multi-part col classes, e.g., POSIX dates with two parts
  col_diffs <- cbind(
    V1 = lapply(dat, class),
    V2 = lapply(dat_out, class)
  ) |>
    tibble::as_tibble(rownames = "var_name") |>
    dplyr::mutate(match_prop = purrr::map_dbl(dat, ~ prop_matching(.x, lvls)))

  types_changed <- purrr::map2_lgl(col_diffs$V1, col_diffs$V2, purrr::negate(identical))

  changed_cols <- col_diffs$var_name[types_changed]

  # ignore.case messages
  if(is.null(ignore.case) && any(match_nocase)) {
    cli::cli_warn(c("!" = "Column{?s} {.var {names(match_nocase)[match_nocase]}} {?was/were} NOT matched, but would match if {.var ignore.case} was set to {.val TRUE}"))
  }

  # message if a match was _already_ a factor, the transformations made, or if no matches
  new_text <- ""
  if(any(col_diffs$match_prop == 1 & purrr::map_lgl(dat, inherits, "factor"))) {
    cli::cli_warn(c("!" = "At least one matching column was already a factor, though this call will have reordered it if different levels were provided. This could be caused by overlapping sets of factor levels, e.g., {.val Yes}, {.val Maybe}, {.val No} and {.val Yes}, {.val No}"))
    new_text <- "new "
  }

  if(length(changed_cols) > 0) {
    cli::cli_inform(c("i" = "Changed the following columns to factors: {.var {changed_cols}}"))
  } else {
    cli::cli_warn(c("!" = "No {new_text}columns matched. Check spelling & capitalization of your levels."))
  }

  tibble::as_tibble(dat_out)
}
