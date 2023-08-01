# Changes from BBC Style:
# - added options for title presence (legend, axis)
# - added options for title / legend / caption alignment (and defaults)
# - added caption and axis title styling (originally empty)
# - added "update_geom_default" for geom_text family
# -

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
                       title_align       = "left",
                       legend_align      = "left",
                       caption_align     = "right") {

  # Check alignment positions for plot title, legend, and caption
  title_align    <- match.arg(title_align,    c("center", "left", "right"))
  legend_align   <- match.arg(legend_align,   c("center", "left", "right"))
  caption_align  <- match.arg(caption_align,  c("center", "left", "right"))

  # Convert text position to a numeric value to supply
  title_h_just    <- switch(title_align,    left = 0, center = 0.5, right = 1)
  caption_h_just  <- switch(caption_align,  left = 0, center = 0.5, right = 1)

  # Update font family and color for geom_text() and geom_label()
  ggplot2::update_geom_defaults("text", list(family = font,
                                    color = "#222222"))
  ggplot2::update_geom_defaults("label", list(family = font,
                                     color = "#222222"))

  result <- ggplot2::theme(

    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family = font,
                                       hjust = title_h_just,
                                       size = 28,
                                       face = "bold",
                                       color = "#222222"),

    #This sets the font, size, type and colour of text for the chart's subtitle,
    #as well as setting a margin between the title and the subtitle
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

    #Legend format
    #This sets the position and alignment of the legend, removes a title and
    #background for it and sets the requirements for any text within the legend.
    #The legend may often need some more manual tweaking when it comes to its
    #exact position based on the plot coordinates.
    legend.position = "top",
    legend.justification = legend_align,
    legend.direction = "horizontal",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(family = font,
                                         size = 20,
                                         color = "#222222"),
    legend.text = ggplot2::element_text(family = font,
                                        size = 18,
                                        color = "#222222"),

    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting
    #the margins and removes lines and ticks. In some cases, axis lines and axis
    #ticks are things we would want to have in the chart - the cookbook shows
    #examples of how to do so.
    axis.title = ggplot2::element_text(family = font,
                                       size = 18,
                                       color = "#222222"),
    axis.text = ggplot2::element_text(family = font,
                                      size = 18,
                                      color = "#222222"),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases
    #you will want to change this to remove y gridlines and add x gridlines. The
    #cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),

    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot
    #background colour from the plot
    panel.background = ggplot2::element_blank(),

    #Strip background
    #This sets the panel background for facet-wrapped plots to
    #white, removing the standard grey ggplot background colour and sets the title
    #size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 22, hjust = 0)
  )

  if(!show_legend_title) {
    result <- result + theme(legend.title = ggplot2::element_blank())
  }

  if(!show_axis_titles) {
    result <- result + theme(axis.title = ggplot2::element_blank())
  }

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
             legend_align = "center")

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
