#' Show TNTP color palette
#'
#' @return
#' @export
#'
#' @examples
show_tntp_colors <- function(palette = "all"){
  if(palette == "likert"){
    scales::show_col(tntpr:::colors_tntp_likert)
  } else if(palette == "likert_orange_to_green"){
    scales::show_col(tntpr:::colors_tntp_likert_orange_to_green)
  } else scales::show_col(tntpr:::colors_tntp)
}
