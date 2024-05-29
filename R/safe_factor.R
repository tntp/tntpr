
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
#' @param ... additional arguments passed on to factor() or ordered()
#'
#' @return a factor
#' @export
#'
#' @examples
#'
#' # No error:
#' tntpr::teacher_survey |>
#'   dplyr::mutate(timing = safe_factor(timing, levels = c("Pre", "Post")))
#'
#' # Mis-typed level generates an error
#' try(
#'   tntpr::teacher_survey |>
#'     dplyr::mutate(timing = safe_factor(timing, levels = c("Pre", "Pst")))
#' )
safe_factor <- function(x = character(), levels, ...) {
  # Still needed: check levels value, deal with no provided levels, write tests
  vals <- unique(x)
  na_vals <- vals[!vals %in% c(levels, NA)]
  if (length(na_vals) > 0) {
    cli::cli_abort("Value{?s} {.val {na_vals}} does not match provided levels.")
  }
  factor(x, levels, ...)
}


#' @export
#' @rdname safe_factor
safe_ordered <- function(x = character(), levels, ...) {
  safe_factor(x, levels, ordered = TRUE, ...)
}
