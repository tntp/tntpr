#' @title Re-install the tntpr package from GitHub.
#' @export
#' @returns nothing
#' @examples
#' \dontrun{
#' # Run without loading tntpr first
#' tntpr::update_tntpr()
#' }
#'

update_tntpr <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop(
      "Package \"devtools\" must be installed to use this function.",
      call. = FALSE
    )
  }

  devtools::install_github("tntp/tntpr")
}
