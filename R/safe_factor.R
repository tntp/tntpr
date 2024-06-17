
#' Set factor levels safely
#'
#' @description
#' The default behavior of `base::factor()` and `base::ordered()` will silently
#' coerce values to NA if they don't match the provided levels.
#'
#' These functions call the base functions, but provide an informative error
#' if any values in the vector don't match a provided level.
#'
#' @param x a vector of data, usually taking a small number of distinct values
#' @param levels a vector of unique values
#' @param update.case Logical. If `TRUE`, will match without checking case, using the capitalization from the levels parameter for the final output.
#' @param ... additional arguments passed on to factor() or ordered()
#'
#' @return a factor
#' @export
#'
#' @examples
#'
#' # No error:
#' teacher_survey |>
#'   dplyr::mutate(timing = safe_factor(timing, levels = c("Pre", "Post")))
#'
#' # Mis-typed level generates an error
#' try(
#'   teacher_survey |>
#'     dplyr::mutate(timing = safe_factor(timing, levels = c("Pre", "Pst")))
#' )
#'
#' # Use update.case to automatically update levels
#' teacher_survey |>
#'   dplyr::mutate(timing = safe_factor(timing, levels = c("pre", "post"),
#'                                      update.case = TRUE))
safe_factor <- function(x = character(), levels,  update.case = FALSE, ...) {

  # If levels aren't provided, no need to run checks
  if (missing(levels)) return(factor(x, ...))

  # Deal with update.case
  if (update.case) {
    x <- standardize_case(x, levels)
  }

  vals <- unique(x)
  na_vals <- vals[!vals %in% c(levels, NA)]
  if (length(na_vals) > 0) {
    msg <- c(
      "x" = "Value{?s} {.val {na_vals}} {?does/do} not match provided levels.",
      "i" = "Provided levels were {.val {levels}}"
    )
    if (!update.case && all(standardize_case(na_vals, levels) %in% levels)) {
      msg <- c(
        msg,
        "i" = "Run with `update.case = TRUE` to automatically re-case data"
      )
    }
    cli::cli_abort(msg)
  }
  factor(x, levels, ...)
}


#' @export
#' @rdname safe_factor
safe_ordered <- function(x = character(), levels, update.case = FALSE, ...) {
  safe_factor(x, levels, update.case, ordered = TRUE, ...)
}
