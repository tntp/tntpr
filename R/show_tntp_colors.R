#' Show TNTP color palette
#'
#'A quick and dirty way to show tntp colors in a plot.
#'
#' @param palette Default is 'all' tntp colors, but can also select a specific
#' palette by passing either 'likert' or 'likert_orange_to_green
#'
#' @return
#' @export
#'
#' @examples show_tntp_colors(palette = "likert")
show_tntp_colors <- function(palette = "all"){
  if(palette == "likert"){
    scales::show_col(tntpr::colors_tntp_likert)
  } else if(palette == "likert_orange_to_green"){
    scales::show_col(tntpr::colors_tntp_likert_orange_to_green)
  } else scales::show_col(tntpr::colors_tntp)
}
