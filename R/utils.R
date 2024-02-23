utils::globalVariables(c("vec", "vec.factor", "n", "group.vec", "group.factor", "perc", "."))

#' Title
#'
#' @param libname library name
#' @param pkgname package name
#'
.onAttach <- function(libname, pkgname) {
  # adapted from hrbrthemes

  if (.Platform$OS.type == "windows") { # nocov start
    windowsFonts <- grDevices::windowsFonts()
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("tntpr.loadfonts", default = FALSE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    pdfFonts <- grDevices::pdfFonts()
    postscriptFonts <- grDevices::postscriptFonts()
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Halyard Display", fnt$FamilyName))) {
    packageStartupMessage("NOTE: The Halyard Display font is the default font for the tntp_style() theme.")
    packageStartupMessage("      This font is not currently installed on your system. ")
    packageStartupMessage("")
    packageStartupMessage("      To install the Halyard Display fonts, follow the directions in the TNTP")
    packageStartupMessage("      Visualization Cookbook at the following link:")
    packageStartupMessage("")
    packageStartupMessage("      https://tntp.github.io/tntpr/articles/visualization-cookbook.html#setting-font-family-and-font-sizes")
  } # nocov end
}
