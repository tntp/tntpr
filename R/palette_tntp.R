#' TNTP branded color palettes
#'
#' This function creates user defined color palettes using any of the
#' following nine TNTP colors: dark_blue, medium_blue, light_blue, green, orange,
#' gold, dark_grey (dark_gray), medium_grey (medium_gray), light_grey
#' (light_gray).
#'@rdname palette_tntp
#'@param ... supply quoted color names to include in color palette
#'@export
#'@examples
#'library(ggplot2)
#'
#' pal1_tntp <- palette_tntp("green", "gold", "orange")
#' pal2_tntp <- palette_tntp("dark_blue", "medium_blue", "light_blue")
#'
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p <- p + geom_point(aes(colour = factor(cyl)))
#' p
#'
#' # Change colors to created palette
#' p <- p + scale_color_manual(values = pal1_tntp)
#' p
#'
#' g <- ggplot(mtcars, aes(factor(cyl), mean(mpg)))
#' g <- g + geom_bar(aes(fill = factor(cyl)), stat = "identity")
#' g
#'
#' # Change fill to created palette
#' g <- g + scale_fill_manual(values = pal2_tntp)
#' g
palette_tntp <- function(...) {

  # TNTP colors
  colors_tntp <- c(dark_blue   = "#034772",
                   medium_blue = "#2888BC",
                   light_blue  = "#73B7CE",
                   green       = "#699D46",
                   orange      = "#EA8936",
                   gold        = "#F9C347",
                   # Support alternative spellings
                   dark_grey   = "#58595B",
                   dark_gray   = "#58595B",
                   medium_grey = "#7D7E81",
                   medium_gray = "#7D7E81",
                   light_grey  = "#C1C2C4",
                   light_gray  = "#C1C2C4")


  # Throw an error if supplied color doesn't exist
  supplied_colors <- c(...)

  if(sum(!is.element(supplied_colors, names(colors_tntp))) > 0) {
    stop("Supplied colors do not exist in TNTP universe, please check spelling")
  }

  # Select the colors from tntp_colors
  color_list <- match.call(expand.dots = TRUE)
  sapply(color_list[-1], function(col) colors_tntp[[as.character(col)]])
}
