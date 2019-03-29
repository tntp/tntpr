#' TNTP branded color palettes
#'
#' This function creates user defined color palette combinations for up to
#' eleven colors. There are nine TNTP approved colors: dark_blue, medium_blue,
#' light_blue, green, orange, gold, dark_grey (dark_gray), medium_grey
#' (medium_gray), light_grey (light_gray). White and black are also available.
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
  colors_tntp


  # Throw an error if supplied color doesn't exist
  supplied_colors <- c(...)

  if(sum(!is.element(supplied_colors, names(colors_tntp))) > 0) {
    stop("Supplied colors do not exist in TNTP universe, please check spelling")
  }

  # Select the colors from tntp_colors
  color_list <- match.call(expand.dots = TRUE)
  sapply(color_list[-1], function(col) colors_tntp[[as.character(col)]])
}


#' TNTP colors
#'
#' @examples
#' \dontrun{
#'  colors_tntp
#' }
#' @export
"colors_tntp"
colors_tntp <- c(# classic colors
                 dark_blue   = "#034772",
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
                 light_gray  = "#C1C2C4",
                 white       = "#FFFFFF",
                 black       = "#000000",

                 # default palette
                 default_1 = "#00a4c7",
                 default_2 = "#ffc72f",
                 default_3 = "#81d2eb",
                 default_4 = "#ea8835",
                 default_5 = "#8ebf3f",
                 default_6 = "#c0c2c4",
                 default_7 = "#006277",
                 default_8 = "#1e97bc",
                 default_9 = "#b58400",
                 default_10 = "#9c5010",
                 default_11 = "#557326",
                 default_12 = "#717478",

                 # likert colors
                 likert_1 = "#ea8835",
                 likert_2 = "#ffc72f",
                 likert_3 = "#ffe9ac",
                 likert_4 = "#e6e7e7",
                 likert_5 = "#cdedf7",
                 likert_6 = "#81d2eb",
                 likert_7 = "#00a4c7")

colors_tntp_classic <- colors_tntp[1:14]
colors_tntp_default <- colors_tntp[15:26]
colors_tntp_likert <- colors_tntp[27:33]

#' scale_palette_tntp
#'
#' @param palette
#'
#' @export
#'
palette_tntp_scales <- function (palette = c("default", "likert_4pt", "likert_5pt",
                                    "likert_6pt", "colors_tntp_classic"))
{
  default <- colors_tntp_default
  likert_4pt <- colors_tntp_likert[rev(c(1, 2, 6, 7))]
  likert_5pt <- colors_tntp_likert[rev(c(1, 2, 4, 6, 7))]
  likert_6pt <- colors_tntp_likert[rev(c(1, 2, 3, 5, 6, 7))]
  colors_tntp_classic <- colors_tntp_classic

  switch(match.arg(palette),
         default = default,
         likert_4pt = likert_4pt,
         likert_5pt = likert_5pt,
         likert_6pt = likert_6pt,
         colors_tntp_classic = colors_tntp_classic)
}


# scale_(fill/color)_tntp functions

#' scale_colour_tntp/scale_color_tntp
#'
#' @param palette character string describing the desired palette
#' @param ... other arguments to pass through to ggplot2::discrete_scale()
#'
#' @export
#'
scale_colour_tntp <- function (palette = c("default", "likert_4pt", "likert_5pt",
                                         "likert_6pt", "colors_tntp_old"), ...)
{
  palette <- match.arg(palette)
  discrete_scale("colour", "tntp", scales::manual_pal(unname(palette_tntp_scales(palette))), ...)
}

#' @rdname scale_colour_tntp
#' @aliases scale_colour_tntp
#' @export


scale_color_tntp <- scale_colour_tntp

#' scale_fill_tntp
#'
#' @param palette One of: "default", "likert_4pt", "likert_5pt", "likert_6pt", "colors_tntp_old"
#' @param ...
#'
#' @export
#'
scale_fill_tntp <- function (palette = c("default", "likert_4pt", "likert_5pt",
                                         "likert_6pt", "colors_tntp_old"), drop = FALSE, ...)
{
  palette <- match.arg(palette)
  discrete_scale("fill", "tntpcolors", scales::manual_pal(unname(palette_tntp_scales(palette))), ...)
}
