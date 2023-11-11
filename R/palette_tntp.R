#' TNTP branded color palettes
#'
#' @md
#' @description
#' This function as been superseded by [tntp_colors()] which has improved
#' functionality and includes the most recent TNTP brand colors.
#'
#' This function creates user defined color palette combinations for up to
#' eleven colors. There are nine TNTP approved colors: dark_blue, medium_blue,
#' light_blue, green, orange, gold, dark_grey (dark_gray), medium_grey
#' (medium_gray), light_grey (light_gray). White and black are also available.
#' @rdname palette_tntp
#' @param ... supply quoted color names to include in color palette
#' @export
#' @examples
#' library(ggplot2)
#'
#' pal1_tntp <- tntp_colors("green", "gold", "orange")
#' pal2_tntp <- tntp_colors("navy", "cerulean", "sky")
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
  .Deprecated('tntp_colors')
  # TNTP colors
  colors_tntp

  # Throw an error if supplied color doesn't exist
  supplied_colors <- c(...)

  if (sum(!is.element(supplied_colors, names(colors_tntp))) > 0) {
    stop("Supplied colors do not exist in TNTP universe, please check spelling")
  }

  # Select the colors from tntp_colors
  color_list <- match.call(expand.dots = TRUE)
  sapply(color_list[-1], function(col) colors_tntp[[as.character(col)]])
}


#' TNTP colors
#'
#' This list of colors has been superseded by the new brand colors and the new
#' function [tntp_colors()].
#'
#' @md
#'
#' @examples
#' \dontrun{
#' tntp_colors()
#' }
#' @export
"colors_tntp"
colors_tntp <- c( # TNTPPalette
  dark_blue = "#00355F",
  medium_blue = "#00A4C7",
  light_blue = "#81D2EB",
  dark_grey = "#404041",
  dark_gray = "#404041", # Support alternative spellings
  medium_grey = "#7D7E81",
  medium_gray = "#7D7E81", # Support alternative spellings
  light_grey = "#C1C2C4",
  light_gray = "#C1C2C4", # Support alternative spellings
  orange = "#EA8835",
  green = "#8EBF3E",
  gold = "#FFC723",
  white = "#FFFFFF",
  black = "#000000",

  # TNTPPaletteExtended
  blue_5 = "#00486B",
  blue_4 = "#007699",
  blue_3 = "#00A4C7",
  blue_2 = "#2DD1F4",
  blue_1 = "#AEFFFF",
  orange_5 = "#8E2C00",
  orange_4 = "#BC5A07",
  orange_3 = "#EA8835",
  orange_2 = "#FFB562",
  orange_1 = "#FFE390",
  green_5 = "#326300",
  green_4 = "#609110",
  green_3 = "#8EBF3E",
  green_2 = "#BBD98B",
  green_1 = "#D1E5B1",
  gold_5 = "#A36B00",
  gold_4 = "#D19900",
  gold_3 = "#FFC72E",
  gold_2 = "#FFF45B",
  gold_1 = "#FFFF89"
)


#' TNTP pallette
#'
#' This list of colors has been superseded by the new brand colors and the new
#' function [tntp_colors()].
#'
#' @md
#' @examples
#' \dontrun{
#' tntp_colors()
#' }
#' @export
"colors_tntp_palette"
colors_tntp_palette <-
  c(
    colors_tntp["medium_blue"],
    colors_tntp["light_blue"],
    colors_tntp["gold"],
    colors_tntp["green"],
    colors_tntp["orange"],
    colors_tntp["light_grey"],
    colors_tntp["dark_blue"],
    colors_tntp["medium_grey"],
    colors_tntp["gold_2"],
    colors_tntp["green_2"],
    colors_tntp["orange_2"],
    colors_tntp["blue_1"],
    colors_tntp["dark_grey"],
    colors_tntp["gold_2"],
    colors_tntp["green_4"],
    colors_tntp["orange_4"]
  )

#' Likert pallette
#'
#' This likert palette has been superseded by the new brand colors and the new
#' function [tntp_palette()].
#'
#' @md
#' @examples
#' \dontrun{
#' tntp_palette('likert_6')
#' }
#' @export
"colors_tntp_likert"
colors_tntp_likert <-
  c(
    colors_tntp["orange_4"],
    colors_tntp["orange_3"],
    colors_tntp["orange_2"],
    colors_tntp["light_grey"],
    colors_tntp["light_blue"],
    colors_tntp["medium_blue"],
    colors_tntp["dark_blue"]
  )

#' Likert orange to green pallette
#'
#' This likert palette has been superseded by the new brand colors and the new
#' functions [tntp_colors()] and [tntp_palette()].
#'
#' @md
#' @examples
#' \dontrun{
#' tntp_palette('bg_6')
#' }
#' @export
"colors_tntp_likert_orange_to_green"
colors_tntp_likert_orange_to_green <-
  c(
    colors_tntp["orange_5"],
    colors_tntp["orange_3"],
    colors_tntp["orange_1"],
    colors_tntp["light_grey"],
    colors_tntp["green_1"],
    colors_tntp["green_3"],
    colors_tntp["green_5"]
  )

#' Pallette names
#'
#' This list of palette names has been superseded by the new brand colors and
#' new functions [tntp_colors()] and [tntp_palette()]. To see all of the new
#' brand palettes, use [show_tntp_palette()].
#'
#' @md
#' @examples
#' \dontrun{
#' show_tntp_palette()
#' }
#' @export
"palette_names"
palette_names <- c(
  "tntp_palette",
  "likert_4pt",
  "likert_5pt",
  "likert_6pt",
  "likert_orange_to_green_4pt",
  "likert_orange_to_green_5pt",
  "likert_orange_to_green_6pt"
)

#' scale_palette_tntp
#'
#' This function has been superseded by [tntp_palette()] which includes the new
#' brand colors.
#'
#' @md
#' @param palette the palette
#'
#' @export
#'
palette_tntp_scales <- function(palette = palette_names) {
  .Deprecated("tntp_palette")
  tntp_palette <- colors_tntp_palette
  likert_4pt <- colors_tntp_likert[rev(c(1, 2, 6, 7))]
  likert_5pt <- colors_tntp_likert[rev(c(1, 2, 4, 6, 7))]
  likert_6pt <- colors_tntp_likert[rev(c(1, 2, 3, 5, 6, 7))]
  likert_orange_to_green_4pt <- colors_tntp_likert_orange_to_green[rev(c(1, 2, 6, 7))]
  likert_orange_to_green_5pt <- colors_tntp_likert_orange_to_green[rev(c(1, 2, 4, 6, 7))]
  likert_orange_to_green_6pt <- colors_tntp_likert_orange_to_green[rev(c(1, 2, 3, 5, 6, 7))]

  switch(match.arg(palette),
    tntp_palette = tntp_palette,
    likert_4pt = likert_4pt,
    likert_5pt = likert_5pt,
    likert_6pt = likert_6pt,
    likert_orange_to_green_4pt = likert_orange_to_green_4pt,
    likert_orange_to_green_5pt = likert_orange_to_green_5pt,
    likert_orange_to_green_6pt = likert_orange_to_green_6pt
  )
}


# scale_(fill/color)_tntp functions

#' scale_colour_tntp/scale_color_tntp
#'
#' This function is deprecated. Please use
#' `scale_color_manual(values = tntp_palette(palette_name))` instead
#'
#' @md
#' @param palette character string describing the desired palette
#' @param ... other arguments to pass through to ggplot2::discrete_scale()
#'
#' @export
#'
scale_colour_tntp <- function(palette = palette_names,
                              drop = FALSE,
                              ...) {
  .Deprecated("tntp_palette")
  palette <- match.arg(palette)
  discrete_scale("colour", "tntp", scales::manual_pal(unname(palette_tntp_scales(palette))), ...)
}

#' @rdname scale_colour_tntp
#' @aliases scale_colour_tntp
#' @param drop drop
#' @param ... other arguments to pass through to ggplot2::discrete_scale()
#' @export
scale_color_tntp <- scale_colour_tntp

#' scale_fill_tntp
#'
#' This function is deprecated. Please use
#' `scale_fill_manual(values = tntp_palette(palette_name))` instead
#'
#' @md
#' @param palette One of: "default", "likert_4pt", "likert_5pt", "likert_6pt", "colors_tntp_old"
#' @param drop drop
#' @param ... additional arguments passed to ggplot2::discrete_scale()
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' x <- mtcars %>%
#'   count(cyl, am) %>%
#'   mutate(am = as.factor(am))
#'
#' ggplot(x, aes(x = cyl, y = n, fill = am)) + # you need a fill aesthetic
#'   geom_col() +
#'   scale_fill_manual(values = tntp_palette())

#'
#' @export
#'
scale_fill_tntp <- function(palette = palette_names,
                            drop = FALSE,
                            ...) {
  .Deprecated("tntp_palette")
  palette <- match.arg(palette)
  discrete_scale("fill", "tntpcolors", scales::manual_pal(unname(palette_tntp_scales(palette))), ...)
}
