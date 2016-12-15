# TITLE: theme_tntp.R
# AUTHOR(S): Alex Spurrier; Jake Russ
# DATE: Unknown
# UPDATED: 2016-06-15

#' TNTP's ggplot2 theme
#'
#' ggplot2 theme customized for TNTP aesthetics
#' @param show_legend_title logical. Should the legend title be shown?  Leave as \code{TRUE} if you want to change the legend title with a subsequent line \code{+ labs(...)}.
#' @param base_size base font size
#' @param base_family base font family
#' @param grid_color color for major gridlines
#' @export
#' @rdname theme_tntp
#' @examples
#' library(ggplot2)
#' library(extrafont)
#'
#' p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour=factor(gear))) + facet_wrap(~am)
#' p
#'
#' # Major gridlines by default
#' p + theme_tntp()
#' # Without gridlines
#' p + theme_tntp(grid_color = "white")

theme_tntp <- function(show_legend_title = TRUE,
                       base_size   = 12,
                       base_family = "Segoe UI",
                       grid_color  = "grey93") {
  # Starts with theme_minimal and then modifies some parts
  result <- theme_minimal(base_size  = base_size, base_family = base_family) %+replace%
    theme(legend.position  = "bottom",        # Move legend to bottom
          legend.key       = element_blank(), # Remove border from legend boxes
          panel.grid.major = element_line(color = grid_color),
          panel.grid.minor = element_blank(), # Remove minor gridlines entirely
          axis.line.y      = element_line(color = "black", size = 0.25),
          axis.line.x      = element_line(color = "black", size = 0.25)
    )
  if(!show_legend_title){
    result <- result + theme(legend.title = element_blank())
  }
  result
}
