# TITLE: theme_tntp.R
# AUTHOR(S): Alex Spurrier; Jake Russ
# DATE: Unknown
# UPDATED: 2016-06-15

#' TNTP's ggplot2 theme
#'
#' This theme is superseded by [tntp_style()]. Ggplot2 theme customized for TNTP aesthetics
#' @param show_legend_title logical. Should the legend title be shown?  Leave as \code{TRUE} if you want to change the legend title with a subsequent line \code{+ labs(...)}.
#' @param base_size base font size
#' @param base_family base font family
#' @param grid_color color for major gridlines
#' @param title_align alignment of main title, defaults to "center"; also accepts "left" or "right"
#' @param title_color color of title text
#' @param title_size size of title text
#' @param subtitle_align alignment of sub-title, defaults to "center"; also accepts "left" or "right"
#' @param subtitle_color color of subtitle text
#' @param subtitle_size size of subtitle text
#' @param caption_align alignment of caption, defaults to "right"; also accepts "left" or "center"
#' @param caption_color color of caption text
#' @param caption_size size of caption text
#' @importFrom ggplot2 %+replace%
#' @export
#' @rdname theme_tntp

theme_tntp <- function(show_legend_title = TRUE,
                       base_size = 12,
                       base_family = "Segoe UI",
                       grid_color = "grey93",
                       title_align = "center",
                       title_color = "black",
                       title_size = 12,
                       subtitle_align = "center",
                       subtitle_color = "black",
                       subtitle_size = 12,
                       caption_align = "right",
                       caption_color = "black",
                       caption_size = 12) {
  # As of v2.2 ggplot2 left aligns titles by default. -------------------------
  .Deprecated("tntp_style")
  # title_align and subtitle_align allows tntpr users to quickly change

  # Check alignment positions for plot title and subtitle
  title_align <- match.arg(title_align, c("center", "left", "right"))
  subtitle_align <- match.arg(subtitle_align, c("center", "left", "right"))
  caption_align <- match.arg(caption_align, c("center", "left", "right"))

  # Convert text position to a numeric value to supply
  title_h_just <- switch(title_align,
    left = 0,
    center = 0.5,
    right = 1
  )
  subtitle_h_just <- switch(subtitle_align,
    left = 0,
    center = 0.5,
    right = 1
  )
  caption_h_just <- switch(caption_align,
    left = 0,
    center = 0.5,
    right = 1
  )

  # Build the theme -----------------------------------------------------------
  # Starts with theme_minimal and then modifies some parts
  result <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) %+replace%
    ggplot2::theme(
      legend.position = "bottom", # Move legend to bottom
      legend.key = ggplot2::element_blank(), # Remove border from legend boxes
      panel.grid.major = ggplot2::element_line(color = grid_color),
      panel.grid.minor = ggplot2::element_blank(), # Remove minor gridlines entirely
      axis.line.y = ggplot2::element_line(color = "black", size = 0.25),
      axis.line.x = ggplot2::element_line(color = "black", size = 0.25),
      plot.title = ggplot2::element_text(
        hjust = title_h_just,
        colour = title_color,
        size = title_size
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = subtitle_h_just,
        colour = subtitle_color,
        size = subtitle_size,
        margin = ggplot2::margin(7)
      ),
      plot.caption = ggplot2::element_text(
        hjust = caption_h_just,
        colour = caption_color,
        size = caption_size
      )
    )
  if (!show_legend_title) {
    result <- result + ggplot2::theme(legend.title = ggplot2::element_blank())
  }
  result
}
