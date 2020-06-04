.onAttach <- function(libname, pkgname) {

import <- "If this is your first time using tntpr on this computer, or you have recently
updated R, please import fonts by running the command font_import() from the extrafont package. Doing so
ensures you have the Segoe UI font loaded."

packageStartupMessage(import, collapse = "\n")

}


