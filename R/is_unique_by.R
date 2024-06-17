

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
#' @param data The input data frame
#' @param ... Columns to check for uniqueness. Takes a tidyselect specification. If no selections are specified, checks for uniqueness by all columns
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
#' library(assertthat)
#'
#' # Testing for unique key values (TRUE)
#' stopifnot(
#'   fake_county |> is_unique_by(tid, school_year)
#' )
#'
#' # Testing for unique key values (FALSE)
#' try(
#'   assert_that(
#'     mtcars |> is_unique_by(cyl, mpg)
#'   )
#' )
#'
is_unique_by <- function(data, ..., .print = TRUE) {

  # Suppress the default "No duplicates found" message
  dupes <- suppressMessages(janitor::get_dupes(data, ...))
  is_unique <- nrow(dupes) == 0
  if (!is_unique && .print) {
    cli::cli_inform(c("i" = "Duplicates shown below"))
    print(dupes)
  }

  is_unique
}

# Custom error message for assert_that
assertthat::on_failure(is_unique_by) <- function(call, env) {
  df_name <- deparse(call$data)
  df_val <- eval(call$data, env)

  # Remove function from call
  arg_list <- as.list(call)[-1]

  # Remove data and .print arguments (if they exist)
  arg_list[c("data", ".print")] <- NULL

  if (length(arg_list) == 0) {
    expr <- rlang::expr(everything())
  } else {
    expr <- rlang::expr(c(!!!arg_list))
  }

  # Get column names with tidyselect. This allows functions like starts_with()
  # to be evaluated first and the resulting columns to be printed
  cols <- tidyselect::eval_select(expr, df_val) |> names()
  cols <- paste0("`", cols, "`")

  paste0(df_name, cli::pluralize(
    " contains duplicate values across the column{?s} {cols}"
  ))
}
