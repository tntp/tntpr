#' Write datatable to temp excel file and open it.
#'
#' @param .data Dataframe
#' @param fileEncoding Encoding
#'
#' @export
#' @return Returns a data.frame.
#'
show_in_excel <- function (.data, fileEncoding = "UTF-8")
{
  tmp <- paste0(tempfile(), ".csv")
  readr::write_excel_csv(.data, tmp, fileEncoding = fileEncoding)
  fs::file_show(path = tmp)
}
