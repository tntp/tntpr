

#' Checks if a font family is usable and returns a usable font if not
#'
#' Helper function. Checks if a given family value is available, and if not returns
#' the default font family ("sans" or user provided)
#'
#' @param family the font family to check as a character
#' @param silent logical. If TRUE doesn't raise a warning if the font family is unavailable
#' @param default_family defaults to "sans", but can be set to another fallback family.
#'
#' @return a character of a usable font family
get_usable_family <- function(family, silent = FALSE, default_family = "sans") {

  # Get a platform-independent list of usable fonts
  if (.Platform$OS.type == "windows") {
    font_list <- names(grDevices::windowsFonts())
  } else {
    font_list <- names(grDevices::quartzFonts())
  }

  # Make sure the default family is available
  if(!default_family %in% font_list) {
    cli::cli_abort(c(
      "x" = "Default family {.val {default_family}} is not registered in the font table.",
      "i" = "Run {.code extrafont::loadfonts()} to register non-core fonts (needs to be done once each session)",
      "i" = "If you've never imported your fonts before, run {.code extrafont::font_import()} first, then {.code extrafont::loadfonts()}"
    ))
  }

  # Check to see if the provided family is available
  if (!family %in% font_list) {
    if(!silent) {
      cli::cli_warn(c(
        "x" = "Family {.val {family}} is not registered in the font table.",
        "v" = "Using standard {.val sans} font instead",
        "i" = "Run {.code extrafont::loadfonts()} to register non-core fonts (needs to be done once each session)",
        "i" = "If you've never imported your fonts before, run {.code extrafont::font_import()} first, then {.code extrafont::loadfonts()}"
      ))
    }

    default_family
  } else {
    family
  }
}


#' Create TNTP themed [ggplot2] charts
#'
#' A custom theme including TNTP fonts and other defaults for styling ggplot2 charts.
#'
#' @md
#'
#' @param family Base font family. Defaults to "Halyard Display".
#' @param header_family Font family for title and subtitle. Defaults to the base font family.
#' @param base_size Base font size. Recommended minimum value of 15.
#' @param text_color Text color for titles, axes, legends, and facets.
#' @param caption_color Text color for caption.
#' @param show_legend_title Logical. Should the legend title be shown?  Leave as `TRUE` if you want to change the legend title with a subsequent line `+ labs(...)`.
#' @param show_axis_titles Which axis titles should be shown? Use `TRUE` or `FALSE` for toggle both titles, or `x` or `y` to show just that axis title.
#' @param grid Which grid lines should be shown? Use `TRUE` or `FALSE` to toggle all grid lines, or a string combination of `X`, `x`, `Y`, `y` for major and minor x and y grid lines.
#' @param grid_color Grid line color.
#' @param title_align,legend_align,caption_align Alignment of title, legend, and caption. Accepts `left`, `right`, or `center`.
#'
#' @export
#'
#' @examples \dontrun{
#' library(tntpr)
#' library(dplyr)
#' library(ggplot2)
#'
#' fake_county |>
#'   filter(t_salary > 0) |>
#'   ggplot(aes(t_experience, t_salary)) +
#'   geom_point() +
#'   scale_y_continuous(labels = scales::dollar) +
#'   labs(
#'     title = "Salary Increases with Experience",
#'     subtitle = "With significant variation at all levels",
#'     x = "Years of Experience",
#'     caption = "Data from the Fake County Data Set"
#'   ) +
#'   tntp_style(show_axis_titles = "x")
#'
#' frpl_experience <- fake_county |>
#'   mutate(frpl_bucket = cut(sch_frpl_pct,
#'     breaks = c(0, 20, 40, 60, 80, 100),
#'     labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")
#'   )) |>
#'   group_by(frpl_bucket) |>
#'   summarize(avg_experience = mean(t_experience, na.rm = TRUE)) |>
#'   mutate(
#'     label = as.character(round(avg_experience, digits = 1)),
#'     label = if_else(frpl_bucket == "0-20%", paste0(label, "\nYears of\nExperience"), label)
#'   )
#'
#' frpl_experience |>
#'   ggplot(aes(frpl_bucket, avg_experience)) +
#'   geom_col(fill = if_else(frpl_experience$frpl_bucket == "60-80%",
#'     tntp_colors("tangerine"),
#'     tntp_colors("medium_gray")
#'   )) +
#'   geom_text(aes(label = label),
#'     nudge_y = -0.25, vjust = 1,
#'     color = "white", size = 5, lineheight = 1,
#'     family = "Segoe UI"
#'   ) +
#'   labs(
#'     title = "High Poverty Schools have Less Experienced Teachers",
#'     x = "% of Student Body Receiving Free/Reduced Lunch"
#'   ) +
#'   scale_y_continuous(breaks = seq(0, 20, 4)) +
#'   tntp_style(
#'     base_size = 20,
#'     show_axis_titles = "x"
#'   )
#' }
#'
tntp_style <- function(family = "Halyard Display",
                       header_family = family,
                       base_size = 28,
                       text_color = "#222222",
                       caption_color = "#7D7E81",
                       show_legend_title = FALSE,
                       show_axis_titles = FALSE,
                       grid = FALSE,
                       grid_color = "#CBCBCB",
                       title_align = "left",
                       legend_align = "left",
                       caption_align = "right") {
  # Minimum text size to display
  min_size <- 6

  # Recommended minimum base size
  recommended_base <- 15

  # Check base_size
  if (!is.numeric(base_size) | base_size <= 0) {
    cli::cli_abort(c("Invalid value {.val {base_size}} for {.var base_size}",
      "i" = "{.var base_size} represents the size of the title text. All other text sizes will scale automatically",
      "i" = "Recommended minimum size is {.val {recommended_base}}"
    ))
  } else if (base_size < recommended_base) {
    cli::cli_warn(c("Values for {.var base_size} below {.val {recommended_base}} are not recommended.",
      "i" = "Current value is {.val {base_size}}"
    ))
  }

  # Scale other graph features to base_size
  title_size <- base_size
  subtitle_size <- base_size * 0.8
  caption_size <- base_size * 0.4
  axis_title_size <- base_size * 0.6

  if (title_size < min_size) title_size <- min_size
  if (subtitle_size < min_size) subtitle_size <- min_size
  if (caption_size < min_size) caption_size <- min_size
  if (axis_title_size < min_size) axis_title_size <- min_size

  facet_size <- caption_size
  legend_size <- axis_title_size

  subtitle_margin <- base_size * 0.3
  axis_text_margin <- base_size * 0.15

  if (!is_color(text_color)) {
    cli::cli_warn(c("Invalid {.var text_color} {.val {text_color}}.",
      "i" = "Using default value of {.val #222222}"
    ))
    text_color <- "#222222"
  }
  if (!is_color(grid_color)) {
    cli::cli_warn(c("Invalid {.var grid_color} {.val {grid_color}}.",
      "i" = "Using default value of {.val #CBCBCB}"
    ))
    grid_color <- "#CBCBCB"
  }
  if (!is_color(caption_color)) {
    cli::cli_warn(c("Invalid {.var caption_color} {.val {caption_color}}.",
      "i" = "Using default value of {.val #7D7E81}"
    ))
    text_color <- "#7D7E81"
  }

  # Check alignment positions for plot title, legend, and caption
  title_align <- rlang::arg_match(title_align, c("center", "left", "right"))
  legend_align <- rlang::arg_match(legend_align, c("center", "left", "right"))
  caption_align <- rlang::arg_match(caption_align, c("center", "left", "right"))

  # Check legend title parameter
  if (!is.logical(show_legend_title)) {
    cli::cli_abort(c("Invalid value {.val {show_legend_title}} for {.var show_legend_title}",
      "i" = "Value should be set to TRUE or FALSE"
    ))
  }
  # Check axis title and grid parameters
  if (!is.logical(show_axis_titles) && !show_axis_titles %in% c("", "x", "y", "xy")) {
    cli::cli_abort(c("Invalid value {.val {show_axis_titles}} for {.var show_axis_titles}",
      "i" = "To show both titles, set to TRUE.",
      "i" = "To hide both titles, set to FALSE.",
      "i" = "To show just x or y set to 'x' or 'y'."
    ))
  }
  if (!is.logical(grid) && !grepl("^[xXyY]+$", grid)) {
    cli::cli_abort(c("Invalid value {.val {grid}} for {.var grid}",
      "i" = "To show all grid-lines, set to TRUE",
      "i" = "To show some grid-lines set to a string combination of X, x, Y, and y, where capital letters represent major grid-lines and lowercase letters represent minor grid-lines."
    ))
  }

  # Check that specified font(s) are available for use
  if(header_family != family) {
    family        <- get_usable_family(family)
    header_family <- get_usable_family(header_family)
  } else {
    family        <- get_usable_family(family)
    header_family <- family
  }

  # Convert text position to a numeric value to supply
  title_h_just <- switch(title_align,
    left = 0,
    center = 0.5,
    right = 1
  )
  caption_h_just <- switch(caption_align,
    left = 0,
    center = 0.5,
    right = 1
  )

  # Create base theme
  result <- ggplot2::theme(

    # Style title, subtitle, and caption. Includes a margin above and below the subtitle
    plot.title = ggplot2::element_text(
      family = header_family,
      hjust = title_h_just,
      size = title_size,
      face = "bold",
      color = text_color
    ),
    plot.subtitle = ggplot2::element_text(
      family = header_family,
      hjust = title_h_just,
      size = subtitle_size,
      color = text_color,
      margin = ggplot2::margin(subtitle_margin, 0, subtitle_margin, 0)
    ),
    plot.caption = ggplot2::element_text(
      family = family,
      hjust = caption_h_just,
      size = caption_size,
      face = "italic",
      color = caption_color
    ),

    # Style legend, including alignment
    legend.position = "top",
    legend.justification = legend_align,
    legend.direction = "horizontal",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(
      family = family,
      size = legend_size,
      color = text_color
    ),
    legend.text = ggplot2::element_text(
      family = family,
      size = legend_size,
      color = text_color
    ),

    # Style axes. Includes a margin on x axis text, no ticks or lines
    axis.title = ggplot2::element_text(
      family = family,
      size = axis_title_size,
      color = text_color
    ),
    axis.text = ggplot2::element_text(
      family = family,
      size = axis_title_size,
      color = text_color
    ),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(axis_text_margin, b = 2 * axis_text_margin)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Style major and minor grid-lines. Grid lines will later be removed if indicated in grid
    panel.grid.minor = ggplot2::element_line(color = grid_color, linewidth = 0.5),
    panel.grid.major = ggplot2::element_line(color = grid_color, linewidth = 1),

    # Style background to blank (gray in standard ggplot2)
    panel.background = ggplot2::element_blank(),

    # Style facet background & title
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(
      family = family,
      hjust = title_h_just,
      size = facet_size,
      color = text_color
    )
  )

  # Toggle for legend title
  if (!show_legend_title) {
    result <- result + ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  # Toggle for axis titles
  if (show_axis_titles == FALSE | show_axis_titles == "") {
    result <- result + ggplot2::theme(axis.title = ggplot2::element_blank())
  } else if (show_axis_titles == "x") {
    result <- result + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  } else if (show_axis_titles == "y") {
    result <- result + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  # Toggle for grid lines
  if (grid == FALSE) {
    result <- result + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  } else if (is.character(grid)) {
    if (!grepl("X", grid)) result <- result + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    if (!grepl("x", grid)) result <- result + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    if (!grepl("Y", grid)) result <- result + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    if (!grepl("y", grid)) result <- result + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  # Return completed theme
  result
}
