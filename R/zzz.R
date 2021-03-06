#' Title
#'
#' @param libname library name
#' @param pkgname package name
#'
#' @return
#' @export
#'
#' @examples
.onAttach <- function(libname, pkgname) {

  # adapted from hrbrthemes

  if (.Platform$OS.type == "windows")  { # nocov start
    windowsFonts <- grDevices::windowsFonts
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("tntpr.loadfonts", default = FALSE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    pdfFonts <- grDevices::pdfFonts
    postscriptFonts <- grDevices::postscriptFonts
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Segoe", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Segoe fonts are required to use this theme.")
    packageStartupMessage("      Please use tntpr::import_fonts() to install Segoe UI")
  } # nocov end

}
