

#' Easy Diverging Bar Charts
#'
#' This is a modification of `ggplot2::position_stack()` for creating diverging
#' bar charts. In order to use this function, you *must* set a fill aesthetic
#' (and that aesthetic should probably be a factor). This function will
#' automatically break your chart into negative and positive values and display
#' them in the same order as your fill levels.
#'
#' @md
#'
#' @param vjust Vertical adjustment for geoms that have a position (like text or points), not a dimension (like bars or areas). Set to 0 to align with the bottom, 0.5 for the middle, and 1 (the default) for the top.
#' @param break_after Either an integer index or character value that represents the last positive level. The default, `NULL`, will split the levels halfway (with fewer positive levels if the total number of levels is odd).
#' @param fill If `TRUE` will automatically scale bars to 100% as with `position_fill()`
#' @param reverse If `TRUE`, will reverse the default stacking order.
#'
#' @importFrom rlang `%||%`
#' @export
#'
#' @examples
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
#' # Default diverging with text
#' # In interactive use, this can also be run with `position = "diverge"`
#'
#' test_df |>
#'   ggplot(aes(prop, q, fill = response)) +
#'   geom_col(position = position_diverge()) +
#'   geom_text(aes(label = scales::percent(prop,)),
#'             position = position_diverge(vjust = 0.5)) +
#'   geom_vline(xintercept = 0) +
#'   tntp_style(family = "sans") +
#'   # Reverse legend to match horizontal bar order
#'   guides(fill = guide_legend(reverse = TRUE)) +
#'   # Adjust axis labels to be positive on both sides
#'   scale_x_continuous(labels = ~scales::percent(abs(.)))
#'
#' # Custom breaks with the break_after parameter
#' test_df |>
#'   ggplot(aes(q, prop, fill = response)) +
#'   geom_col(position = position_diverge(break_after = 'Yes')) +
#'   geom_hline(yintercept = 0) +
#'   tntp_style(family = "sans") +
#'   # Adjust axis labels to be positive on both sides
#'   scale_y_continuous(labels = ~scales::percent(abs(.)))
#'
position_diverge <- function(vjust = 1, break_after = NULL, fill = FALSE,
                             reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionDiverge, vjust = vjust,
                   break_after = break_after, fill = fill, reverse = reverse)
}

PositionDiverge <- ggplot2::ggproto("PositionDiverge", ggplot2::PositionStack,
  break_after = NULL,

  # This doesn't seem to work?
  required_aes = "fill",

  setup_params = function(self, data) {
    # Copied from PositionStack
    flipped_aes <- ggplot2::has_flipped_aes(data)
    data <- ggplot2::flip_data(data, flipped_aes)

    # New
    # Check lvls
    if (is.factor(data$fill)) {
      lvls <- levels(data$fill)
    } else if(!is.null(data$fill)) {
      lvls <- sort(unique(data$fill))
      cli::cli_inform(c(
        "i" = "For best results, use a factor for the {.var fill} aesthetic with `position_diverge()`"
      ))
    } else {
      cli::cli_abort(c(
        "x" = "`position_diverge()` requires a provided {.var fill} aesthetic."
      ))
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
      # From PositionStack
      var = self$var %||% ggplot2:::stack_var(data),
      fill = self$fill,
      vjust = self$vjust,
      reverse = self$reverse,
      flipped_aes = flipped_aes,
      # New
      break_after = break_after,
      lvls = lvls
    )
  },

  compute_panel = function(data, params, scales) {
    data <- ggplot2::flip_data(data, params$flipped_aes)

    # Check/adjust for data positivity
    y_vals <- intersect(c("y", "ymin", "ymax"), names(data))
    if (any(unlist(data[y_vals]) < 0)) {
      cli::cli_warn(c(
        "!" = "Data contains negative plotting values.",
        "i" = "Values have been coerced to positive for plotting with `position_diverge()`"
        ))

      data[y_vals] <- lapply(data[y_vals], abs)
    }

    # Store original data order
    data$order <- seq_len(nrow(data))

    # Order data temporarily, reversing if needed
    if (params$reverse) {
      data <- dplyr::arrange(data, x, fill)
      break_lvls <- params$lvls[1:params$break_after]
    } else {
      data <- dplyr::arrange(data, x, desc(fill))
      break_lvls <- params$lvls[(params$break_after + 1):length(params$lvls)]
    }

    # Stack ymax
    data$ymax <- data$ymax |>
      split(data$x) |>
      lapply(cumsum) |>
      unlist()

    # Scale (if fill = TRUE)
    if (params$fill) {
      data$ymax <- data$ymax |>
        split(data$x) |>
        lapply(\(v) v / max(v)) |>
        unlist()
    }

    # Set ymin
    data$ymin <- data$ymax |>
      split(data$x) |>
      lapply(\(v) c(0, v[-length(v)])) |>
      unlist()

    # Set y, adjusting for vjust
    data$y <- data$ymin + params$vjust * (data$ymax - data$ymin)

    # Break at break_after
    data <- data |>
      split(~x) |>
      lapply(\(df) {
        if (any(df$fill %in% break_lvls)) {
          b_max <- max(df$ymax[df$fill %in% break_lvls])
        } else {
          b_max <- 0
        }
        df[c("ymax", "ymin", "y")] <- df[c("ymax", "ymin", "y")] - b_max
        df
      }) |>
      do.call(rbind, args = _)

    # Restore original order
    data <- data[order(data$order), ]
    data$order <- NULL

    # Return data (flipped back if necessary)
    ggplot2::flip_data(data, params$flipped_aes)
  }

)
