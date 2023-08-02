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
# - axis title formatting? (alignment? size?)



#' Add TNTP theme to ggplot chart
#'
#' This function allows you to add the TNTP theme to your ggplot graphics.
#' @keywords tntp_style
#' @export

tntp_style <- function(font = "Segoe UI",
                       show_legend_title = FALSE,
                       show_axis_titles  = FALSE,
                       grid              = FALSE,
                       title_align       = "left",
                       legend_align      = "left",
                       caption_align     = "right") {

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
                                       hjust = title_h_just,
                                       size = 28,
                                       face = "bold",
                                       color = "#222222"),

    plot.subtitle = ggplot2::element_text(family = font,
                                          hjust = title_h_just,
                                          size = 22,
                                          color = "#222222",
                                          margin = ggplot2::margin(9,0,9,0)),

    plot.caption = ggplot2::element_text(family = font,
                                         hjust = caption_h_just,
                                         size = 12,
                                         face = "italic",
                                         color = "#7D7E81"),

    # Style legend, including alignment
    legend.position = "top",
    legend.justification = legend_align,
    legend.direction = "horizontal",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(family = font,
                                         size = 18,
                                         color = "#222222"),
    legend.text = ggplot2::element_text(family = font,
                                        size = 18,
                                        color = "#222222"),

    # Style axes. Includes a margin on x axis text, no ticks or lines
    axis.title = ggplot2::element_text(family = font,
                                       size = 18,
                                       color = "#222222"),
    axis.text = ggplot2::element_text(family = font,
                                      size = 18,
                                      color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Style major and minor grid-lines. Grid lines will later be removed if indicated in grid
    panel.grid.minor = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.5),
    panel.grid.major = ggplot2::element_line(color = "#cbcbcb", linewidth = 1),

    # Style background to blank (gray in standard ggplot2)
    panel.background = ggplot2::element_blank(),

    # Style facet background & title
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(family = font,
                                       hjust = title_h_just,
                                       size = 22,
                                       color = "#222222")
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

# Tests / examples
library(ggplot2)
library(dplyr)


# Scatterplot
ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point(size = 3) +
  labs(x = "Engine Displacement", y = "MPG", color = "Class:",
       title = "Seminal ggplot2 scatterplot example",
       subtitle = "A plot that is only useful for demonstration purposes",
       caption = "Brought to you by the letter 'g'") +
  scale_y_continuous(limits = c(0, 50)) +
  geom_hline(yintercept = 0, linewidth = 1, color = "#333333") +
  tntp_style(show_axis_titles = TRUE,
             show_legend_title = TRUE,
             legend_align = "center",
             grid = 'Yy') +
  facet_wrap(~drv)

# Bar Chart
count(mpg, class) |>
  ggplot(aes(class, n)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 3) +
  labs(x = "", y = "",
       title = "Seminal ggplot2 bar chart example",
       subtitle = "A plot that is only useful for demonstration purposes",
       caption = "Brought to you by the letter 'g'") +
  tntp_style()

ggsave("test3.svg", width = 9, height = 6)

