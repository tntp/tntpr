#' @title Re-install the tntpr package from GitHub.
#' @export

update_tntpr <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop(
      "Package \"devtools\" must be installed to use this function.",
      call. = FALSE
    )
  }

  devtools::install_github("tntp/tntpr")
}
