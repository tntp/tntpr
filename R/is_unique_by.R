

#' Check a data frame for uniquely identifying columns
#'
#' @description
#' This function extends `janitor::get_dupes()` into an assertion. If duplicate
#' values exist for the specified columns of the data frame it returns false
#' and prints the duplicates (printing can be disabled with the `.print`
#' parameter)
#'
#' This function is pipe-friendly and designed to be used with `stopifnot()` or
#' `assertthat::assert_that()`
#'
#' @md
#'
#' @param df The input data frame
#' @param ... Columns to check for uniqueness. Takes a tidyselect specification
#' @param .print Should duplicates be printed if they exist? Defaults to TRUE
#'
#' @return returns a logical value. If the data frame is NOT uniquely
#' identified by the given columns, prints the duplicates to the console
#' as well.
#'
#' @export
#'
#' @examples
#'
#' # Testing for unique key values (TRUE)
#' stopifnot(
#'   fake_county |> is_unique_by(tid, school_year)
#' )
#'
#' # Testing for unique key values (FALSE)
#' try(
#'   stopifnot(
#'     wisc |> is_unique_by(district, school, grade)
#'   )
#' )
#'
is_unique_by <- function(df, ..., .print = TRUE) {

  # Suppress the default "No duplicates found" message
  dupes <- suppressMessages(janitor::get_dupes(df, ...))
  is_unique <- nrow(dupes) == 0
  if (!is_unique && .print) {
    cli::cli_inform(c("i" = "Duplicates shown below"))
    print(dupes)
  }

  is_unique
}

# Custom error message for assert_that
assertthat::on_failure(is_unique_by) <- function(call, env) {
  df <- deparse(call$df)
  args <- rlang::call_args(call)
  cols <- as.character(args[!names(args) %in% c("df", ".print")])
  cols <- paste0("`", cols, "`")

  cli::pluralize("{df} contains duplicate values of the columns {cols}")
}
