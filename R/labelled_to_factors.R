#' @title Convert all `labelled`-class columns to factors.
#'
#' @md
#'
#' @description
#' Deprecated. Use the `as_factor()` function from the `haven` package instead for the same functionality.
#'
#' Takes a data.frame, checks for columns that are class `labelled` from the `haven` package, and converts them to factor class.
#'
#' @param labels_df a data.frame containing some columns of class labelled
#' @returns Returns a data.frame, the same size as `labels_df`
#' @export
#' @examples
#'
#' tntpr::fake_county |>
#'   haven::as_factor()
#'
labelled_to_factors <- function(labels_df) {

  .Deprecated()

  labeled_var_index <- unlist(
    lapply(labels_df, function(x) class(x) == "labelled")
  )
  factorized <- labels_df
  factorized[, labeled_var_index] <- lapply(factorized[, labeled_var_index], as.factor)


  # reset label attributes - maybe not needed, and not working
  question_text <- unlist(
    lapply(raw, function(x) {
      attr(x, "label")
    })
  )

  for (i in seq_along(factorized)) {
    attr(factorized[[i]], "label") <- question_text[names(factorized)[i]]
  }

  factorized
}
