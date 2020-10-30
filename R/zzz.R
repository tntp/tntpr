#' Loadfonts when the package loads
#'
#' @param libname
#' @param pkgname
#'
#' @return
#' @export
#'
#' @examples
.onAttach <- function(libname, pkgname) {

  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("tntpr.loadfonts", default = FALSE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Segoe", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Segoe fonts are required to use this theme.")
    packageStartupMessage("      Please use tntpr::import_segoe_ui() to install Segoe UI")
  } # nocov end

}
