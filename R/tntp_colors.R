
#' TNTP Brand Colors
#'
#' Translate human friendly TNTP brand color names like "medium_blue" into
#' accurate hex values for use in plotting. This function can also be used to
#' show a named vector of all available TNTP brand colors and values. Use
#' `show_tntp_colors()` to quickly visualize selected colors in the plot window.
#' For often used palettes of TNTP colors, see [tntp_palette()].
#'
#' @param ... Supply quoted TNTP color names to return. If no colors are specified, returns all available colors.
#' @param pattern Optional regular expression. If provided, will return only brand colors that match the regular expression
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
#'   geom_point(color = tntp_colors('green'))
#'
#' #... multiple colors ...
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_manual(values = tntp_colors('green', 'orange', 'red'))
#'
#' #... or a list of all possible TNTP brand colors
#' tntp_colors()
#'
#' # Use show_tntp_colors() to quickly see brand colors in the plotting window
#' show_tntp_colors('light_green', 'medium_green', 'green')
#'
#' # You can also use a pattern to return similar colors
#' show_tntp_colors(pattern = 'green')
#'
#' # You can see all colors (and names) by running it with no arguments
#' show_tntp_colors()

tntp_colors <- function(...) {

    tntp_color_list <- c(

      # Primary
      black       = '#000000',
      light_gray  = '#F1F1EE',
      light_green = '#E2EDDC',
      white       = '#FFFFFF',

      # Secondary
      medium_yellow = '#FDE57B',
      light_red     = '#FDDACA',
      medium_blue   = '#00A5C7',
      red           = '#C31F46',
      green         = '#317D5C',

      # Extended
      blue         = '#00355F',
      orange       = '#F26C4C',
      gray         = '#4A4A4A',
      yellow       = '#F2CF13',
      light_blue   = '#81D3EB',
      medium_red   = '#DA8988',
      medium_green = '#8FB09D',

      # NOT IN BRAND PALETTE
      light_yellow = '#FAEDB8',
      medium_gray  = '#A5A5A5',

      # For 5-scale
      green_4 = "#60977D",
      green_2 = "#B9CFBD",
      blue_4  = "#006D93",
      blue_2  = "#41BCD9",
      red_4   = "#CF5467",
      red_2   = "#ECB2A9",
      yellow_4 = "#F8DA47",
      yellow_2 = "#FCE99A",
      gray_4  = "#787878",
      gray_2  = "#CBCBCA"

  )

  supplied_colors <- c(...)

  # Return full color list with names if run with no arguments
  if(is.null(supplied_colors)) {
    return(tntp_color_list)
  }

  # Adjust for gray/grey spelling
  supplied_colors <- gsub("grey", "gray", supplied_colors)

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
show_tntp_colors <- function(..., pattern = NULL, labels = TRUE, borders = NULL,
                             cex_label = 1, ncol = NULL) {


  # Validate parameters
  if(!is.logical(labels)) {
    cli::cli_warn(c("!" = "Invalid {.var labels} value of {.val {labels}}",
                    "i" = "{.var labels} accepts values of {.val TRUE} or {.val FALSE}",
                    "i" = "Defaulting to {.val TRUE}"))
    labels <- TRUE
  }
  # Validate color inputs
  is_color <- function(x) {
    res <- try(grDevices::col2rgb(x), silent = TRUE)
    return(!"try-error" %in% class(res))
  }

  if(!is_color(borders)) {
    cli::cli_warn(c("!" = "Invalid {.var borders} value of {.val {borders}}",
                    "i" = "{.var borders} accepts any color value or {.val NA}",
                    "i" = "Defaulting to {.val {par(\"fg\")}}"))
    borders <- par("fg")
  }

  # Code adapted from scales::show_col but with text labels in addition to hex codes
  colours <- tntp_colors(...)

  # Filter using the pattern, if provided
  if(!is.null(pattern)) {
    colours <- colours[grepl(pattern, names(colours))]
    if(length(colours) == 0) {
      cli::cli_abort(c("!" = "No colors match the pattern {.val {pattern}}",
                       "i" = "Run {.run tntpr::show_tntp_colors()} to see available colors"))
    }
  }

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
  "colorful" = tntp_colors("green", "red", "blue", "yellow", "orange"),
  "likert_4" = tntp_colors("yellow", "medium_yellow", "medium_green", "green"),
  "likert_5" = tntp_colors("yellow", "medium_yellow", "light_gray", "medium_green", "green"),
  "likert_6" = tntp_colors("yellow", "medium_yellow", "light_yellow", "light_green", "medium_green", "green"),
  "likert_7" = tntp_colors("yellow", "medium_yellow", "light_yellow", "light_gray", "light_green", "medium_green", "green"),
  "rb_4" = tntp_colors("red", "medium_red", "medium_blue", "blue"),
  "rb_5" = tntp_colors("red", "medium_red", "light_gray", "medium_blue", "blue"),
  "rb_6" = tntp_colors("red", "medium_red", "light_red", "light_blue", "medium_blue", "blue"),
  "rb_7" = tntp_colors("red", "medium_red", "light_red", "light_gray", "light_blue", "medium_blue", "blue"),
  "bg_4" = tntp_colors("blue", "medium_blue", "medium_green", "green"),
  "bg_5" = tntp_colors("blue", "medium_blue", "light_gray", "medium_green", "green"),
  "bg_6" = tntp_colors("blue", "medium_blue", "light_blue", "light_green", "medium_green", "green"),
  "bg_7" = tntp_colors("blue", "medium_blue", "light_blue", "light_gray", "light_green", "medium_green", "green"),
  "greens" = tntp_colors("green", "green_4", "medium_green", "green_2", "light_green"),
  "reds" = tntp_colors("red", "red_4", "medium_red", "red_2", "light_red"),
  "blues" = tntp_colors("blue", "blue_4", "medium_blue", "blue_2", "light_blue"),
  "yellows" = tntp_colors("yellow", "yellow_4", "medium_yellow", "yellow_2", "light_yellow"),
  "grays" = tntp_colors("gray", "gray_4", "medium_gray", "gray_2", "light_gray")
  )

#' Common TNTP Color Palettes
#'
#' Use or see
#'
#' @param palette Name of the TNTP palette you want to use. To see all available palettes, use `show_tntp_palette()`
#' @param ... Supply quoted TNTP palette names to visualize. If no names are specified, shows all available palettes.
#' @param reverse Logical. If set to `TRUE`, reverses the direction of the palette.
#' @param pattern Optional regular expression. If provided, will return only palettes that match the regular expression
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
#' show_tntp_palette('bg_5', 'likert_5')
#'
#' # You can use a pattern to show similar palettes
#' show_tntp_palette(pattern = 'bg')
#'
#' # Or run it with no specified palettes to see all available palettes
#' show_tntp_palette()
#'
#' # For creating a continuous color palette, use scale_color_gradient()
#' # along with tntp_colors():
#' ggplot(diamonds, aes(depth, table, color = price)) +
#'   geom_point() +
#'   scale_color_gradient(low = tntp_colors('light_green'),
#'                        high = tntp_colors('green'))
#'

tntp_palette <- function(palette = "likert_6", reverse = FALSE) {

    pal <- tntp_palette_list[[palette]]

  if(is.null(pal)) {
    cli::cli_abort(c("x" = "No TNTP palette found for {.val {palette}}",
                     "i" = "Run {.run tntpr::show_tntp_palette()} to see available palettes",
                     "i" = "You can also use {.code tntp_colors()} to create your own palette from available TNTP colors"))
  }

  if(reverse) rev(pal) else pal

}

#' @rdname tntp_palette
show_tntp_palette <- function(..., reverse = FALSE, pattern = NULL) {

  palettes <- c(...)

  # Return an error if arguments include unmatched palettes
  unmatched_pals <- palettes[!palettes %in% names(tntp_palette_list)]
  if(length(unmatched_pals) > 0) {
    cli::cli_abort(c("!" = "No match for the following palette name(s)",
                     "x" = paste0("{.val ", unmatched_pals, "}", collapse = ", "),
                     "i" = "Run {.run tntpr::show_tntp_palette()} to see available palettes"))
  }

  if(is.null(palettes)) palettes <- names(tntp_palette_list)

  # Filter by pattern if it exists
  if(!is.null(pattern)) {
    palettes <- palettes[grepl(pattern, palettes)]
    if(length(palettes) == 0) {
      cli::cli_abort(c("!" = "No palettes match the pattern {.val {pattern}}",
                       "i" = "Run {.run tntpr::show_tntp_palette()} to see available palettes"))
    }
  }

  max_length <- max(lengths(tntp_palette_list)[palettes])

  colours <- lapply(palettes,  function(pal_name) {
    pal <- tntp_palette_list[[pal_name]]
    if(reverse) pal <- rev(pal)
    names(pal) <- NULL
    c(pal, rep(NA, max_length - length(pal)))
  }) |> unlist() |> matrix(ncol = max_length, byrow = TRUE)


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
