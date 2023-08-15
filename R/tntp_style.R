# Changes from BBC Style:
# - added options for title presence (legend, axis)
# - added options for title / legend / caption alignment (and defaults)
# - added options for grid lines
# - added options for font
# - added caption and legend/axis title styling (originally empty)
# - adjusted facet-title styling

# Unresolved:
# - font management (2018)?

# Questions:
# - do we add all the parameters with proper defaults? OR use + theme()
#   - Include "most used" parameters, don't include others.
#   - Add base_size to scale all text sizes
# - function name: keep as tntp_style() or match tntp_theme_xxxx() or theme_tntp()

# Next steps - get to polished draft stage, then get feedback from folks.
# - We start using / testing. Think through questions above
# - Sam adds / drafts documentation
# - Finalize next meeting, then roll out to data team.




#' Add TNTP theme to ggplot chart
#'
#' This function allows you to add the TNTP theme to your ggplot graphics.
#' @keywords tntp_style
#' @export

tntp_style <- function(font              = "Segoe UI",
                       base_size         = 28,
                       text_color        = "#222222",
                       caption_color     = "#7D7E81",
                       show_legend_title = FALSE,
                       show_axis_titles  = FALSE,
                       grid              = FALSE,
                       grid_color        = "#CBCBCB",
                       title_align       = "left",
                       legend_align      = "left",
                       caption_align     = "right") {

  # Minimum text size to display
  min_size         <- 6

  # Recommended minimum base size
  recommended_base <- 15

  # Check base_size
  if(!is.numeric(base_size) | base_size <= 0) {
    cli::cli_abort(c("Invalid value {.val {base_size}} for {.var base_size}",
                     "i" = "{.var base_size} represents the size of the title text. All other text sizes will scale automatically",
                     "i" = "Recommended minimum size is {.val {recommended_base}}"))
  } else if(base_size < recommended_base) {
    cli::cli_warn(c("Values for {.var base_size} below {.val {recommended_base}} are not recommended.",
                  "i" = "Current value is {.val {base_size}}"))
  }

  # Scale other graph features to base_size
  title_size      <- base_size
  subtitle_size   <- base_size * 0.8
  caption_size    <- base_size * 0.4
  axis_title_size <- base_size * 0.6

  if(title_size      < min_size) title_size      <- min_size
  if(subtitle_size   < min_size) subtitle_size   <- min_size
  if(caption_size    < min_size) caption_size    <- min_size
  if(axis_title_size < min_size) axis_title_size <- min_size

  facet_size  <- caption_size
  legend_size <- axis_title_size

  subtitle_margin  <- base_size * 0.3
  axis_text_margin <- base_size * 0.15

  # Validate color inputs
  is_color <- function(x) {
    res <- try(col2rgb(x), silent = TRUE)
    return(!"try-error" %in% class(res))
  }
  if(!is_color(text_color)) {
    cli::cli_warn(c("Invalid {.var text_color} {.val {text_color}}.",
                    "i" = "Using default value of {.val #222222}"))
    text_color <- "#222222"
  }
  if(!is_color(grid_color)) {
    cli::cli_warn(c("Invalid {.var grid_color} {.val {grid_color}}.",
                    "i" = "Using default value of {.val #CBCBCB}"))
    grid_color <- "#CBCBCB"
  }
  if(!is_color(caption_color)) {
    cli::cli_warn(c("Invalid {.var caption_color} {.val {caption_color}}.",
                    "i" = "Using default value of {.val #7D7E81}"))
    text_color <- "#7D7E81"
  }

  # Check alignment positions for plot title, legend, and caption
  title_align    <- rlang::arg_match(title_align,    c("center", "left", "right"))
  legend_align   <- rlang::arg_match(legend_align,   c("center", "left", "right"))
  caption_align  <- rlang::arg_match(caption_align,  c("center", "left", "right"))

  # Check legend title parameter
  if(!is.logical(show_legend_title)) {
    cli::cli_abort(c("Invalid value {.val {show_legend_title}} for {.var show_legend_title}",
                     "i" = "Value should be set to TRUE or FALSE"))
  }
  # Check axis title and grid parameters
  if(!is.logical(show_axis_titles) && !show_axis_titles %in% c('x', 'y', 'xy')) {
    cli::cli_abort(c("Invalid value {.val {show_axis_titles}} for {.var show_axis_titles}",
                     "i" = "To show both titles, set to TRUE.",
                     "i" = "To hide both titles, set to FALSE.",
                     "i" = "To show just x or y set to 'x' or 'y'."))
  }
  if(!is.logical(grid) && !grepl('^[xXyY]+$', grid)) {
    cli::cli_abort(c("Invalid value {.val {grid}} for {.var grid}",
                     "i" = "To show all grid-lines, set to TRUE",
                     "i" = "To show some grid-lines set to a string combination of X, x, Y, and y, where capital letters represent major grid-lines and lowercase letters represent minor grid-lines."))
  }

  # Check that specified font is available for use
  if(!font %in% names(windowsFonts())) {
    cli::cli_warn(c("x" = "Font {.val {font}} is not registered in the font table.",
                    "v" = "Using standard {.val sans} font instead",
                    "i" = "Run {.code extrafont::loadfonts()} to register non-core fonts (needs to be done once each session)",
                    "i" = "If you've never imported your fonts before, run {.code extrafont::font_import()} first, then {.code extrafont::loadfonts()}"))
    font <- 'sans'
  }

  # Convert text position to a numeric value to supply
  title_h_just    <- switch(title_align,    left = 0, center = 0.5, right = 1)
  caption_h_just  <- switch(caption_align,  left = 0, center = 0.5, right = 1)

  # Update font family and color for geom_text() and geom_label()
  # ggplot2::update_geom_defaults("text", list(family = font,
  #                                   color = "#222222"))
  # ggplot2::update_geom_defaults("label", list(family = font,
  #                                    color = "#222222"))

  # Create base theme
  result <- ggplot2::theme(

    # Style title, subtitle, and caption. Includes a margin above and below the subtitle
    plot.title = ggplot2::element_text(family = font,
                                       hjust  = title_h_just,
                                       size   = title_size,
                                       face   = "bold",
                                       color  = text_color),

    plot.subtitle = ggplot2::element_text(family = font,
                                          hjust  = title_h_just,
                                          size   = subtitle_size,
                                          color  = text_color,
                                          margin = ggplot2::margin(subtitle_margin, 0, subtitle_margin, 0)),

    plot.caption = ggplot2::element_text(family = font,
                                         hjust  = caption_h_just,
                                         size   = caption_size,
                                         face   = "italic",
                                         color  = caption_color),

    # Style legend, including alignment
    legend.position = "top",
    legend.justification = legend_align,
    legend.direction = "horizontal",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(family = font,
                                         size   = legend_size,
                                         color  = text_color),
    legend.text = ggplot2::element_text(family = font,
                                        size   = legend_size,
                                        color  = text_color),

    # Style axes. Includes a margin on x axis text, no ticks or lines
    axis.title = ggplot2::element_text(family = font,
                                       size = axis_title_size,
                                       color = text_color),
    axis.text = ggplot2::element_text(family = font,
                                      size = axis_title_size,
                                      color = text_color),
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
    strip.text = ggplot2::element_text(family = font,
                                       hjust = title_h_just,
                                       size = facet_size,
                                       color = text_color)
  )

  # Toggle for legend title
  if(!show_legend_title) {
    result <- result + ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  # Toggle for axis titles
  if(show_axis_titles == FALSE) {
    result <- result + ggplot2::theme(axis.title = ggplot2::element_blank())
  } else if(show_axis_titles == 'x') {
    result <- result + ggplot2::theme(axis.title.y = ggplot2::element_blank())
  } else if(show_axis_titles == 'y') {
    result <- result + ggplot2::theme(axis.title.x = ggplot2::element_blank())
  }

  # Toggle for grid lines
  if(grid == FALSE) {
    result <- result + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                      panel.grid.minor = ggplot2::element_blank())
  } else if(is.character(grid)) {
    if(!grepl('X', grid)) result <- result + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
    if(!grepl('x', grid)) result <- result + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
    if(!grepl('Y', grid)) result <- result + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
    if(!grepl('y', grid)) result <- result + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
  }

  # Return completed theme
  result

}

# # Tests / examples
# library(ggplot2)
# library(dplyr)
#
#
# # Scatterplot
# ggplot(mpg, aes(displ, hwy, color = class)) +
#   geom_point(size = 3) +
#   labs(x = "Engine Displacement", y = "MPG", color = "Class:",
#        title = "Seminal ggplot2 scatterplot example",
#        subtitle = "A plot that is only useful for demonstration purposes",
#        caption = "Brought to you by the letter 'g'") +
#   scale_y_continuous(limits = c(0, 50)) +
#   geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
#   tntp_style(show_axis_titles = TRUE,
#              show_legend_title = TRUE,
#              legend_align = "center",
#              grid = 'Yy',
#              base_size = 15,
#              text_color = tntpr::palette_tntp("dark_blue")) +
#   facet_wrap(~drv)
#
# # Bar Chart
# count(mpg, class) |>
#   ggplot(aes(class, n)) +
#   geom_col() +
#   geom_text(aes(label = n), nudge_y = 3) +
#   labs(x = "", y = "",
#        title = "Seminal ggplot2 bar chart example",
#        subtitle = "A plot that is only useful for demonstration purposes",
#        caption = "Brought to you by the letter 'g'") +
#   tntp_style()
#
# ggsave("test3.svg", width = 9, height = 6)

