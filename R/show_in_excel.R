#' Write datatable to temp excel file and open it.
#'
#' @param .data Dataframe
#'
#' @export
#' @return Returns a data.frame.
#'
show_in_excel <- function (.data)
{
  tmp <- paste0(tempfile(), ".csv")
  write.csv(.data, tmp, fileEncoding = "UTF-8")
  fs::file_show(path = tmp)
}
