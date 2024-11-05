utils::globalVariables(c("vec", "vec.factor", "n", "group.vec", "group.factor", "perc", "."))

#' Title
#'
#' @param libname library name
#' @param pkgname package name
#'
.onAttach <- function(libname, pkgname) {
  # adapted from hrbrthemes
  msg <- character()

  if (.Platform$OS.type == "windows") { # nocov start
    windowsFonts <- grDevices::windowsFonts()
    if (interactive()) msg <- c("v" = "Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("tntpr.loadfonts", default = FALSE)) {
    if (interactive()) msg <- c("v" = "Registering PDF & PostScript fonts with R")
    pdfFonts <- grDevices::pdfFonts()
    postscriptFonts <- grDevices::postscriptFonts()
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Halyard Display", fnt$FamilyName))) {
    msg <- c(msg,
      "!" = "The {.val Halyard Display} font is the default font for the {.code tntp_style()} theme. This font is not currently installed on your system.",
      "i" = "To install {.val Halyard Display}, follow the directions in the {.href [TNTP Visualization Cookbook](https://tntp.github.io/tntpr/articles/visualization-cookbook.html#setting-font-family-and-font-sizes)}."
    )
    # packageStartupMessage("NOTE: The 'Halyard Display' font is the default font for the tntp_style() theme. This font is not currently installed on your system. ")
    # packageStartupMessage("")
    # packageStartupMessage("      To install 'Halyard Display', follow the directions in the TNTP Visualization Cookbook at the following link:")
    # packageStartupMessage("")
    # packageStartupMessage("      https://tntp.github.io/tntpr/articles/visualization-cookbook.html#setting-font-family-and-font-sizes")
  } # nocov end

  if (interactive()) {
    cli::cli_rule(left = "{.pkg tntpr} {utils::packageVersion('tntpr')}")
    cli::cli_inform(msg, class = "packageStartupMessage")
  }
}
