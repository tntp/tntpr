

#' Easy Diverging Bar Charts
#'
#' This is a modification of `ggplot2::position_stack()` for creating diverging bar charts.
#' In order to use this function, you must set a fill aesthetic and that aesthetic must be
#' a factor. This function will automatically break your chart into negative and positive
#' values and display them in the same order as geom_col()
#'
#' @param vjust OPTIONAL set to 0.5 for geom_text() or geom_label() to center within the bar.
#' @param break_after OPTIONAL. Either an integer index or character value that is the last positive level
#'
#' @importFrom rlang `%||%`
#' @export
#'
#' @examples \dontrun{
#' library(ggplot2)
#'
#' # Example data
#' test_df <- tibble::tribble(
#'   ~q,  ~response,  ~prop,
#'   'a', 'Yes',      0.25,
#'   'a', 'Mostly',   0.25,
#'   'a', 'Somewhat', 0.25,
#'   'a', 'Not Yet',  0.25,
#'   'b', 'Yes',      0.4,
#'   'b', 'Mostly',   0.3,
#'   'b', 'Somewhat', 0.2,
#'   'b', 'Not Yet',  0.1
#'   ) |>
#'   dplyr::mutate(
#'     response = forcats::fct_inorder(response),
#'     q = forcats::fct_inorder(q)
#'   )
#'
#' # Default diverging (splits in half)
#' test_df |>
#'   ggplot(aes(q, prop, fill = response)) +
#'   geom_col(position = position_diverge()) +
#'   geom_text(aes(label = scales::percent(prop,)), position = position_diverge(vjust = 0.5)) +
#'   geom_hline(yintercept = 0) +
#'   tntp_style() +
#'   # Adjust axis labels to be positive on both sides
#'   scale_y_continuous(labels = ~scales::percent(abs(.)))
#'
#' # break_after paramater
#' test_df |>
#'   ggplot(aes(q, prop, fill = response)) +
#'   geom_col(position = position_diverge(break_after = 'Yes')) +
#'   geom_hline(yintercept = 0) +
#'   tntp_style() +
#'   # Adjust axis labels to be positive on both sides
#'   scale_y_continuous(labels = ~scales::percent(abs(.)))
#' }
#'
position_diverge <- function(vjust = 1, break_after = NULL) {
  ggplot2::ggproto(NULL, PositionDiverge, vjust = vjust, break_after = break_after)
}

PositionDiverge <- ggplot2::ggproto("PositionDiverge", ggplot2::Position,
                           type = NULL,
                           vjust = 1,
                           fill = FALSE,
                           break_after = NULL,

                           setup_params = function(self, data) {
                             flipped_aes <- ggplot2::has_flipped_aes(data)
                             data <- ggplot2::flip_data(data, flipped_aes)

                             # Pull lvls
                             if(is.factor(data$fill)) {
                               lvls <- levels(data$fill)
                             } else if(!is.null(data$fill)){
                               lvls <- sort(unique(data$fill))
                               cli::cli_inform(c("i" = "For best results, use a factor for the {.var fill} aesthetic with `position_diverge()`"))
                             } else {
                               cli::cli_abort(c("x" = "`position_diverge()` requires a provided {.var fill} aesthetic."))
                             }

                             # Use length over 2 if break_after isn't provided
                             break_after <- self$break_after %||% floor(length(lvls) / 2)

                             # Parse character provision of break_after
                             if(is.character(break_after)) {
                               char <- break_after
                               break_after <- which(lvls == break_after)
                               # If value isn't found
                               if(length(break_after) == 0) {
                                 cli::cli_abort(c("x" = "Provided break_after level {.val {char}} not found in the levels for the fill variable",
                                                  "i" = "Fill variable levels are {.val {lvls}}"))
                               }
                             }

                             list(
                               var = self$var %||% ggplot2:::stack_var(data),
                               fill = self$fill,
                               vjust = self$vjust,
                               flipped_aes = flipped_aes,
                               break_after = break_after,
                               lvls = lvls
                             )
                           },

                           setup_data = function(self, data, params) {
                             data <- ggplot2::flip_data(data, params$flipped_aes)
                             if (is.null(params$var)) {
                               return(data)
                             }

                             data$ymax <- switch(params$var,
                                                 y = data$y,
                                                 ymax = as.numeric(ifelse(data$ymax == 0, data$ymin, data$ymax))
                             )

                             vars <- intersect(c("x", "xmin", "xmax", "y"), names(data))
                             missing <- ggplot2:::detect_missing(data, vars)
                             data[missing, vars] <- NA

                             # Make data negative if it's before the break
                             lvls_p <- params$lvls[1:params$break_after]
                             if('y'    %in% names(data)) data$y    <- ifelse(data$fill %in% lvls_p, data$y,    -data$y)
                             if('ymax' %in% names(data)) data$ymax <- ifelse(data$fill %in% lvls_p, data$ymax, -data$ymax)
                             if('ymin' %in% names(data)) data$ymin <- ifelse(data$fill %in% lvls_p, data$ymin, -data$ymin)

                             flip_data(data, params$flipped_aes)
                           },

                           compute_panel = function(data, params, scales) {
                             data <- ggplot2::flip_data(data, params$flipped_aes)
                             if (is.null(params$var)) {
                               return(data)
                             }

                             negative <- data$ymax < 0
                             negative[is.na(negative)] <- FALSE

                             neg <- data[negative, , drop = FALSE]
                             pos <- data[!negative, , drop = FALSE]

                             if (any(negative)) {
                               neg <- ggplot2:::collide(neg, NULL, "position_stack", ggplot2:::pos_stack,
                                                        vjust = params$vjust,
                                                        fill = params$fill,
                                                        # Reverse the negative side (undoes the inside-out stacking)
                                                        reverse = TRUE
                               )
                             }
                             if (!all(negative)) {
                               pos <- ggplot2:::collide(pos, NULL, "position_stack", ggplot2:::pos_stack,
                                                        vjust = params$vjust,
                                                        fill = params$fill,
                                                        # Don't reverse the positive side
                                                        reverse = FALSE
                               )
                             }

                             data <- ggplot2:::vec_rbind0(neg, pos)[match(seq_len(nrow(data)), c(which(negative), which(!negative))),]
                             ggplot2::flip_data(data, params$flipped_aes)
                           }
)
