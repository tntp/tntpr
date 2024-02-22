#' Bar chart of counts with TNTP polish
#'
#' Takes a user supplied data frame and turns the designated column into
#' an N bar chart (uses position dodge from ggplot2).
#' @rdname bar_chart_counts
#' @param df the data.frame to be used in the bar chart
#' @param var unquoted column name for variable to count
#' @param group_var (optional) unquoted column name for group variable.  If this is specified, you get a 2-variable clustered bar chart.  If left blank, a single variable bar chart.
#' @param labels should labels show the count (\code{"n"}) or the percentage (\code{"pct"})?
#' @param var_color color for non-grouped charts; set to TNTP green by default. For both this and \code{group_colors}, strings will be tried in \code{tntp_colors} automatically.  So \code{c("red", "green")} will get you the official TNTP colors, while \code{c("red", "brown")} will get you base R red and blue.
#' @param group_colors character vector of group colors, if a specific palette is desired
#' @param title main chart title
#' @param var_label label for x-axis
#' @param digits integer indicating the number of decimal places to be used in percentages. In truncating, ties are rounded up, like in MS Excel, i.e., 10.5 and 11.5 become 11 and 12.  This is *not* base R's default behavior.
#' @param font font for chart text; Segoe UI by default
#' @param font_size size for chart text; set to 12 by default
#' @export
#' @examples
#'
#' # An N bar chart by default
#' # All examples use font = "sans" to avoid triggering font warnings
#' mtcars |>
#'   bar_chart_counts(var   = cyl,
#'                    var_color = "orange",
#'                    title = "Number of mtcars by cylinder",
#'                    font  = "sans")
#'
#' # Use a grouping variable with custom colors
#' mtcars |>
#'   bar_chart_counts(var       = cyl,
#'                    group_var = vs,
#'                    group_colors = c("orange", "navy"),
#'                    labels    = "pct",
#'                    title     = "% of V vs. Straight engines by # of cylinders",
#'                    font  = "sans")
#"

bar_chart_counts <- function(df,
                             var,
                             group_var = NULL,
                             labels = "n",
                             var_color = "green",
                             group_colors = NULL,
                             title = NULL,
                             var_label = NULL,
                             digits = 1,
                             font = "Halyard Display",
                             font_size = 12) {
  # QC: Throw an error if object supplied to df is not a data.frame -----------
  if (!is.data.frame(df)) {
    stop("You must supply a data.frame to the df argument")
  }

  # QC: Throw an error if var was not specified  ------------------------------
  if (missing(var)) {
    stop("You must supply a column name to the var argument")
  }

  # Ensure the specified font is valid
  font <- get_usable_family(font)

  # Store whether there is a grouping variable
  grouped <- !rlang::quo_is_null(rlang::enquo(group_var))

  # Create a plot_data object -------------------------------------------------
  # plot_data should contain user specified column and its factor equivalent

  # Check if a grouping variable was specified
  if (!grouped) {
    plot_data <- df |>
      dplyr::select(vec = {{var}}) |>
      dplyr::mutate(vec.factor = as.factor(vec)) |>
      dplyr::group_by(vec.factor) |>
      dplyr::tally() |>
      dplyr::mutate(perc = n / sum(n))
  } else {
    plot_data <- df |>
      dplyr::select(vec = {{var}},
                    group.vec = {{group_var}}) |>
      dplyr::mutate(
        vec.factor = as.factor(vec),
        group.factor = as.factor(group.vec)
      ) |>
      dplyr::group_by(vec.factor, group.factor) |>
      dplyr::tally() |>
      dplyr::group_by(vec.factor) |>
      dplyr::mutate(perc = n / sum(n)) |>
      dplyr::ungroup() |>
      tidyr::complete(vec.factor, group.factor, fill = list(n = NA, perc = NA))
  }

  # Create a color palette ----------------------------------------------------

  # Check if group_var is supplied
  if (grouped) {
    if (is.null(group_colors)) {
      # QC: If group_var is supplied, but no colors, create a color palette
      #     while also making sure there are enough colors for each group

      # Select a color for each level of the factor-ed grouping variable. Must
      # be less than or equal to 11 (because we only have 11 TNTP colors)
      num_groups <- plot_data$group.factor |>
        levels() |>
        length()

      if (num_groups > 7) {
        stop("The maximum number of levels in group_var with the default color palette is 7")
      }

      # Use the new colorful palette by default
      tntp_col_pal <- tntp_palette('colorful')

    } else {
      # QC: Throw an error if the number of levels in supplied group_var does
      # not equal the number of group_colors
      num_group_var <- plot_data$group.factor |>
        levels() |>
        length()
      num_group_col <- group_colors |> length()

      if (num_group_var != num_group_col) {
        stop("The number of group_colors must equal
                                 the number of levels supplied to group_var")
      }

      # Match provided color names to either TNTP or base R colors
      tntp_col_pal <- swap_colors(group_colors)
    }
  }

  # Check whether user specified an x axis label ------------------------------
  if (is.null(var_label)) {
    var_label <- deparse(substitute(var))
  }

  # Build the N bar chart -----------------------------------------------------
  # Condition on presence of group_var

  if (!grouped) {
    if (labels == "pct") {
      nbc <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = vec.factor, y = perc)) +
        ggplot2::geom_bar(fill = swap_colors(var_color), stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(janitor::round_half_up(perc, digits + 2), digits = digits)),
          vjust = -0.8,
          family = font,
          size = font_size * 0.35
        ) # different ratio for font size in geom_text vs. element, see http://stackoverflow.com/a/25062509
    } else {
      nbc <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = vec.factor, y = n)) +
        ggplot2::geom_bar(fill = swap_colors(var_color), stat = "identity") +
        ggplot2::geom_text(
          mapping = ggplot2::aes(label = n),
          vjust = -0.8,
          family = font,
          size = font_size * 0.35
        )
    }
  } else {
    if (labels == "pct") {
      nbc <- ggplot2::ggplot(
        data = plot_data,
        mapping = ggplot2::aes(x = vec.factor, y = perc, fill = group.factor)
      ) +
        ggplot2::geom_bar(position = "dodge", stat = "identity", na.rm = TRUE) + # silences warnings when there's an empty bar because of a subgroup of size 0
        ggplot2::scale_fill_manual(values = tntp_col_pal)


      nbc <- nbc +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(janitor::round_half_up(perc, digits + 2), digits = digits)),
          position = ggplot2::position_dodge(width = 0.9),
          vjust = -0.8,
          na.rm = TRUE,
          family = font,
          size = font_size * 0.35
        )
    } else {
      nbc <- ggplot2::ggplot(
        data = plot_data,
        mapping = ggplot2::aes(x = vec.factor, y = n, fill = group.factor)
      ) +
        ggplot2::geom_bar(position = "dodge", stat = "identity", na.rm = TRUE) + # silences warnings when there's an empty bar because of a subgroup of size 0
        ggplot2::scale_fill_manual(values = tntp_col_pal)


      nbc <- nbc +
        ggplot2::geom_text(ggplot2::aes(label = n),
          position = ggplot2::position_dodge(width = 0.9),
          vjust = -0.8,
          na.rm = TRUE,
          family = font,
          size = font_size * 0.35
        )
    }
  }

  # Polish the plot to presentation standards ---------------------------------

  # so labels don't get cropped, set the y scale 10% higher than the highest bar
  max_height <- dplyr::if_else(labels == "pct",
    max(plot_data$perc, na.rm = TRUE) * 1.1,
    max(plot_data$n, na.rm = TRUE) * 1.1
  )
  nbc <- nbc +
    ggplot2::scale_y_continuous(
      expand = c(0, 0), # do people mind there being no whitespace at the bottom?  There's none in Excel
      limits = c(0, max_height)
    ) +
    ggplot2::labs(title = title, x = var_label) +
    tntp_style(family = font) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
  nbc
}

# function to swap in custom TNTP colors
swap_colors <- function(x) {
  # Attempt to match to TNTP colors first
  tryCatch(tntp_colors(x),
           error = \(e) {
             # Attempt to test all values as R colors next
             if(is_color(x)) {
               cli::cli_inform(c("i" = "Unable to map some colors from {.val {x}} to TNTP colors. Using base R colors instead."))
               x
             } else {
               cli::cli_warn(c("!" = "Unable to map some colors from {.val {x}} to either TNTP colors or base R colors.",
                               "i" = "Using values from {.code tntp_palette('colorful')} instead."))
               tntp_palette('colorful')
             }

           })
}
