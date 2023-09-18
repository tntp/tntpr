#' Write datatable to temp excel file and open it.
#'
#' @param .data Dataframe
#'
#' @export
#' @return Returns a data.frame.
#'
show_in_excel <- function(.data) {
  tmp <- paste0(tempfile(), ".csv")
  readr::write_excel_csv(.data, tmp)
  fs::file_show(path = tmp)
}
