
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
#'   scale_color_manual(values = tntp_colors('green', 'navy', 'red'))
#'
#' #... or a list of all possible TNTP brand colors
#' tntp_colors()
#'
#' # Use show_tntp_colors() to quickly see brand colors in the plotting window
#' show_tntp_colors('mint', 'moss', 'green')
#'
#' # You can also use a pattern to return similar colors
#' show_tntp_colors(pattern = 'green')
#'
#' # You can see all colors (and names) by running it with no arguments
#' show_tntp_colors()

tntp_colors <- function(...) {

    tntp_color_list <- c(

      # Primary
      mint        = '#E2EDDC',
      light_green = '#E2EDDC',
      gray        = '#F1F1EE',
      grey        = '#F1F1EE',
      light_gray  = '#F1F1EE',
      light_grey  = '#F1F1EE',
      white       = '#FFFFFF',
      black       = '#000000',

      # Secondary
      yellow        = '#FDE57B',
      medium_yellow = '#FDE57B',
      navy          = '#00355F',
      blue          = '#00355F',
      pink          = '#FDDACA',
      light_red     = '#FDDACA',
      red           = '#C31F46',
      green         = '#317D5C',

      # Extended
      charcoal     = '#4A4A4A',
      dark_gray    = '#4A4A4A',
      dark_grey    = '#4A4A4A',
      tangerine    = '#F26C4C',
      orange       = '#F26C4C',
      salmon       = '#DA8988',
      medium_red   = '#DA8988',
      gold         = '#F2CF13',
      moss         = '#8FB09D',
      medium_green = '#8FB09D',
      sky          = '#81D3EB',
      light_blue   = '#81D3EB',
      cerulean     = '#00A5C7',
      medium_blue  = '#00A5C7',

      # NOT IN BRAND PALETTE
      light_yellow = '#FAEDB8',
      medium_gray  = '#9E9E9C',
      medium_grey  = '#9E9E9C',
      dark_gold    = '#C2A60A',
      dark_green   = '#1D4935',
      dark_red     = '#7C132C',

      # For 5-scale
      green_4 = "#60977D",
      green_2 = "#B9CFBD",
      blue_4  = "#006D93",
      blue_2  = "#41BCD9",
      red_4   = "#CF5467",
      red_2   = "#ECB2A9",
      yellow_4 = "#F8DA47",
      yellow_2 = "#FCE99A",
      gray_4  = "#747473",
      grey_4  = "#747473",
      gray_2  = "#C7C7C5",
      grey_2  = "#C7C7C5"

  )

  supplied_colors <- c(...)

  # Return full color list with names if run with no arguments
  if(is.null(supplied_colors)) {

    # Consolidate names for colors that appear in the list multiple times
    consolidated_colors <- unique(tntp_color_list)
    consolidated_names <- character(length(consolidated_colors))

    for(i in seq_along(consolidated_colors)) {
      consolidated_names[i] <- paste0(names(tntp_color_list)[tntp_color_list == consolidated_colors[i]], collapse = "|")
    }

    names(consolidated_colors) <- consolidated_names

    return(consolidated_colors)
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

# Validate color inputs
is_color <- function(x) {
  res <- try(grDevices::col2rgb(x), silent = TRUE)
  return(!"try-error" %in% class(res))
}

# Choose a text color given a background color
choose_text_color <- function(bg_color) {
  stopifnot(is_color(bg_color))
  ifelse(colSums(grDevices::col2rgb(bg_color) * c(.299, .587, .114)) > 150,
     "black", "white")
}


#' @export
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


  if(!is_color(borders)) {
    cli::cli_warn(c("!" = "Invalid {.var borders} value of {.val {borders}}",
                    "i" = "{.var borders} accepts any color value or {.val NA}",
                    "i" = "Defaulting to {.val {par(\"fg\")}}"))
    borders <- graphics::par("fg")
  }

  # Code adapted from scales::show_col but with text labels in addition to hex codes
  colours <- tntp_colors(...)

  # If the full list was pulled
  if(is.null(c(...))) {
    # Format consolidated names for printing
    names(colours) <- gsub("|", "\n", names(colours), fixed = TRUE)
  } else {
    # Otherwise add back in selected names
    names(colours) <- c(...)
  }

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
  old <- graphics::par(pty = "s", mar = c(0, 0, 0, 0))
  on.exit(graphics::par(old))
  size <- max(dim(colours))
  plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  graphics::rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
       col = colours, border = borders)
  if (labels) {
    label_col <- choose_text_color(colours)
    graphics::text(x = col(colours) - 0.5,
                   y = -row(colours) + 0.5,
                   labels = color_labels,
                   cex = cex_label,
                   col = label_col)
  }
}

# Private variable
tntp_palette_list <- list(
  "colorful" = tntp_colors("green", "gold", "navy", "red", "charcoal", "tangerine", "black"),
  "likert_4" = tntp_colors("dark_gold", "gold", "green", "dark_green"),
  "likert_5" = tntp_colors("dark_gold", "gold", "gray_2", "green", "dark_green"),
  "likert_6" = tntp_colors("dark_gold", "gold", "yellow", "moss", "green", "dark_green"),
  "likert_7" = tntp_colors("dark_gold", "gold", "yellow", "gray_2", "moss", "green", "dark_green"),
  "top2_4" = tntp_colors("gray_2", "gray", "green", "dark_green"),
  "top2_5" = tntp_colors("medium_gray", "gray_2", "gray", "green", "dark_green"),
  "top2_6" = tntp_colors("gray_4", "medium_gray", "gray_2", "gray", "green", "dark_green"),
  "top2_7" = tntp_colors("charcoal", "gray_4", "medium_gray", "gray_2", "gray", "green", "dark_green"),
  "rb_4" = tntp_colors("dark_red", "red", "cerulean", "navy"),
  "rb_5" = tntp_colors("dark_red", "red", "gray_2", "cerulean", "navy"),
  "rb_6" = tntp_colors("dark_red", "red", "salmon", "sky", "cerulean", "navy"),
  "rb_7" = tntp_colors("dark_red", "red", "salmon", "gray_2", "sky", "cerulean", "navy"),
  "bg_4" = tntp_colors("navy", "cerulean", "green", "dark_green"),
  "bg_5" = tntp_colors("navy", "cerulean", "gray_2", "green", "dark_green"),
  "bg_6" = tntp_colors("navy", "cerulean", "sky", "moss", "green", "dark_green"),
  "bg_7" = tntp_colors("navy", "cerulean", "sky", "gray_2", "moss", "green", "dark_green"),
  "greens" = tntp_colors("green", "green_4", "medium_green", "green_2", "light_green"),
  "reds" = tntp_colors("red", "red_4", "medium_red", "red_2", "light_red"),
  "blues" = tntp_colors("blue", "blue_4", "medium_blue", "blue_2", "light_blue"),
  "yellows" = tntp_colors("gold", "yellow_4", "medium_yellow", "yellow_2", "light_yellow"),
  "grays" = tntp_colors("dark_gray", "gray_4", "medium_gray", "gray_2", "light_gray"),
  "nps" = tntp_colors("red", "red", "medium_red", "medium_red", "light_red", "light_red", "medium_yellow", "medium_yellow", "green", "green")
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
#'   scale_fill_manual(values = tntp_palette('blues', reverse = TRUE))
#'
#'
#' # Use show_tntp_palette() to visualize a single or multiple palettes
#' show_tntp_palette('likert_7')
#' show_tntp_palette('bg_5', 'likert_5')
#'
#' # You can use a pattern to show similar palettes
#' show_tntp_palette(pattern = 'top2')
#' show_tntp_palette(pattern = '_6')
#'
#' # Or run it with no specified palettes to see all available palettes
#' show_tntp_palette()
#'
#' # For creating a continuous color palette, use scale_color_gradient()
#' # along with tntp_colors():
#' ggplot(mtcars, aes(hp, disp, color = mpg)) +
#'   geom_point(size = 3) +
#'   scale_color_gradient(low = tntp_colors('red'),
#'                        high = tntp_colors('green'))


tntp_palette <- function(palette = "likert_6", reverse = FALSE) {

  pal <- tntp_palette_list[[palette]]

  if(is.null(pal)) {
    cli::cli_abort(c("x" = "No TNTP palette found for {.val {palette}}",
                     "i" = "Run {.run tntpr::show_tntp_palette()} to see available palettes",
                     "i" = "You can also use {.code tntp_colors()} to create your own palette from available TNTP colors"))
  }

  if(reverse) rev(pal) else pal

}

#' @export
#' @rdname tntp_palette
show_tntp_palette <- function(..., reverse = FALSE, pattern = NULL) {

  palettes <- c(...)

  # If run with no arguments, show all palettes
  if(is.null(palettes)) palettes <- names(tntp_palette_list)

  # Return an error if arguments include unmatched palettes
  unmatched_pals <- palettes[!palettes %in% names(tntp_palette_list)]
  if(length(unmatched_pals) > 0) {
    cli::cli_abort(c("!" = "No match for the following palette name(s)",
                     "x" = paste0("{.val ", unmatched_pals, "}", collapse = ", "),
                     "i" = "Run {.run tntpr::show_tntp_palette()} to see available palettes"))
  }

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


  old <- graphics::par(pty = "m", mar = c(0, 0, 0, 0))
  on.exit(graphics::par(old))
  size <- max(dim(colours))
  size_x <- dim(colours)[2]
  size_y <- dim(colours)[1]
  size_label <- ceiling(size_x / 2)
  plot(c(0, size_label + size_x), c(0, -size_y), type = "n", xlab = "", ylab = "",
       axes = FALSE)
  graphics::rect(col(colours) - 1 + size_label, -row(colours) + 1, col(colours) + size_label, -row(colours),
       col = colours, border = FALSE)
  graphics::text(x = size_label - 0.25,
       y = -seq(1, length(palettes)) + 0.5,
       adj = 1,
       labels = palettes,
       col = "black")

}
