
#' TNTP Brand Colors
#'
#' Translate human friendly TNTP brand color names like "medium_blue" into
#' accurate hex values for use in plotting. This function can also be used to
#' show a named vector of all available TNTP brand colors and values. Use
#' `show_tntp_colors()` to quickly visualize selected colors in the plot window.
#' For often used palettes of TNTP colors, see [tntp_palette()].
#'
#' @param ... Supply quoted TNTP color names to return. If no colors are specified, returns all available colors.
#' @param labels Logical. Label colors with names and hex values?
#' @param borders Border color for each tile. Default uses `par("fg")`. Use `border = NA` to omit borders.
#' @param cex_label Size of printed labels, as multiplier of default size.
#' @param ncol Number of columns. If not supplied, tries to be as square as possible.
#'
#' @export
#'
#' @md
#' @examples
#'
#' library(ggplot2)
#'
#' # Use tntp_colors() to retrieve a single color...
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point(color = tntp_colors('medium_blue'))
#'
#' #... multiple colors ...
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_manual(values = tntp_colors('medium_blue', 'orange', 'green'))
#'
#' #... or a list of all possible TNTP brand colors
#' tntp_colors()
#'
#' # Use show_tntp_colors() to quickly see brand colors in the plotting window
#' show_tntp_colors('blue_1', 'blue_2', 'blue_3', 'blue_4', 'blue_5')
#'
#' # You can see all colors (and names) by running it with no arguments
#' show_tntp_colors

tntp_colors <- function(...) {

    tntp_color_list <- c(
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
    gold_1 = "#FFFF89",
    black = "#000000",
    white = "#FFFFFF"
  )

  supplied_colors <- c(...)

  # Return full color list if run with no arguments
  if(is.null(supplied_colors)) {
    return(tntp_color_list)
  }

  # Return an error if arguments include unmatched colors
  unmatched_colors <- supplied_colors[!supplied_colors %in% names(tntp_color_list)]
  if(length(unmatched_colors) > 0) {
    cli::cli_abort(c("!" = "No match for the following color name(s)",
                     "x" = paste0("{.val ", unmatched_colors, "}", collapse = ", "),
                     "i" = "Run {.run tntpr::show_tntp_colors()} to see available colors"))
  }

  # Return supplied colors. Unname for use in scale_color_manual and other functions.
  tntp_color_list[supplied_colors] |> unname()
}

#' @rdname tntp_colors
show_tntp_colors <- function(..., labels = TRUE, borders = NULL, cex_label = 1,
                             ncol = NULL) {

  # Code adapted from scales::show_col but with text labels in addition to hex codes
  colours <- tntp_colors(...)

  n <- length(colours)
  ncol <- ifelse(is.null(ncol), ceiling(sqrt(length(colours))), ncol)
  nrow <- ceiling(n/ncol)
  colours <- c(colours, rep(NA, nrow * ncol - length(colours)))

  # Create label: name + hex value
  color_labels <- paste0(names(colours), "\n", colours)
  color_labels[color_labels == "\nNA"] <- ""

  colours <- matrix(colours, ncol = ncol, byrow = TRUE)
  color_labels <- matrix(color_labels, ncol = ncol, byrow = TRUE)
  old <- par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colours))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
       col = colours, border = borders)
  if (labels) {
    hcl <- farver::decode_colour(colours, "rgb", "hcl")
    label_col <- ifelse(hcl[, "l"] > 50, "black", "white")
    text(x = col(colours) - 0.5,
         y = -row(colours) + 0.5,
         labels = color_labels,
         cex = cex_label,
         col = label_col)
  }
}

# Private variable
tntp_palette_list <- list(
  "likert_4" = tntp_colors("dark_blue", "medium_blue", "orange_3", "orange_4"),
  "likert_5" = tntp_colors("dark_blue", "medium_blue", "light_gray", "orange_3", "orange_4"),
  "likert_6" = tntp_colors("dark_blue", "medium_blue", "light_blue", "orange_2", "orange_3", "orange_4"),
  "likert_7" = tntp_colors("dark_blue", "medium_blue", "light_blue", "light_gray", "orange_2", "orange_3", "orange_4"),
  "og_4" = tntp_colors("orange_4", "orange_3", "green_3", "green_4"),
  "og_5" = tntp_colors("orange_4", "orange_3", "light_gray", "green_3", "green_4"),
  "og_6" = tntp_colors("orange_4", "orange_3", "orange_2", "green_2", "green_3", "green_4"),
  "og_7" = tntp_colors("orange_4", "orange_3", "orange_2", "light_gray", "green_2", "green_3", "green_4")
  )

#' Common TNTP Color Palettes
#'
#' Use or see
#'
#' @param palette Name of the TNTP palette you want to use. To see all available palettes, use `show_tntp_palette()`
#' @param ... Supply quoted TNTP palette names to visualize. If no names are specified, shows all available palettes.
#' @param reverse Logical. If set to `TRUE`, reverses the direction of the palette.
#'
#' @export
#' @md
#'
#' @examples
#'
#' library(ggplot2)
#'
#' # Use to add a common palette to a ggplot visualization
#' ggplot(diamonds, aes(y = color, fill = cut)) +
#'   geom_bar(position = "fill") +
#'   scale_fill_manual(values = tntp_palette('likert_5', reverse = TRUE))
#'
#'
#' # Use show_tntp_palette() to visualize a single or multiple palettes
#' show_tntp_palette('likert_7')
#' show_tntp_palette('og_5', 'likert_5')
#'
#'
#' # Or run it with no specified palettes to see all available palettes
#' show_tntp_palette()

tntp_palette <- function(palette = "likert_6", reverse = FALSE) {

    pal <- tntp_palette_list[[palette]]

  if(is.null(pal)) {
    cli::cli_abort(c("x" = "No TNTP palette found for {.val {palette}}",
                     "i" = "Run {.run tntpr::show_tntp_palette()} to see available palettes",
                     "i" = "You can also use {.code tntp_colors()} to create your own palette"))
  }

  if(reverse) rev(pal) else pal

}

#' @rdname tntp_palette
show_tntp_palette <- function(..., reverse = FALSE) {

  palettes <- c(...)
  if(is.null(palettes)) palettes <- names(tntp_palette_list)

  max_length <- max(lengths(tntp_palette_list)[palettes])

  colours <- lapply(palettes,  function(pal_name) {
    pal <- tntp_palette_list[[pal_name]]
    if(reverse) pal <- rev(pal)
    names(pal) <- NULL
    c(pal, rep(NA, max_length - length(pal)))
  }) |> unlist() |> matrix(ncol = max_length, byrow = TRUE)

  # Labeling should be cleaned up (not pretty right now)
  old <- par(pty = "m", mar = c(0, 0, 0, 0))
  on.exit(par(old))
  size <- max(dim(colours))
  size_x <- dim(colours)[2]
  size_y <- dim(colours)[1]
  size_label <- ceiling(size_x / 2)
  plot(c(0, size_label + size_x), c(0, -size_y), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  rect(col(colours) - 1 + size_label, -row(colours) + 1, col(colours) + size_label, -row(colours),
       col = colours, border = FALSE)
  text(x = size_label - 0.25,
       y = -seq(1, length(palettes)) + 0.5,
       adj = 1,
       labels = palettes,
       col = "black")

}
