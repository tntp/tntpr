#' Write Dataframe to a temp excel file and open it.
#'
#' @param .data Dataframe
#'
#' @export
#' @returns nothing
#' @examples
#' \dontrun{
#' # View a data set in excel
#' mtcars |> show_in_excel()
#' }
#'
#'
show_in_excel <- function(.data) {
  tmp <- paste0(tempfile(), ".csv")
  readr::write_excel_csv(.data, tmp)
  utils::browseURL(url = tmp)
}
