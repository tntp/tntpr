#' Bar chart of counts with TNTP polish
#'
#' Takes a user supplied data frame and turns the designated column into
#' an N bar chart (uses position dodge from ggplot2).
#' @rdname bar_chart_counts
#' @param df the data.frame to be used in the bar chart
#' @param var unquoted column name for variable to count
#' @param group_var (optional) unquoted column name for group variable.  If this is specified, you get a 2-variable clustered bar chart.  If left blank, a single variable bar chart.
#' @param labels should labels show the count (\code{"n"}) or the percentage (\code{"pct"})?
#' @param var_color color for non-grouped charts; set to medium_blue by default. For both this and \code{group_colors}, strings will be tried in \code{palette_tntp} automatically.  So \code{c("orange", "dark_blue")} will get you the official TNTP colors, while \code{c("orange", "blue")} will get you TNTP orange but generic blue.
#' @param group_colors character vector of group colors, if a specific palette is desired
#' @param title main chart title
#' @param var_label label for x-axis
#' @param digits integer indicating the number of decimal places to be used in percentages. In truncating, ties are rounded up, like in MS Excel, i.e., 10.5 and 11.5 become 11 and 12.  This is *not* base R's default behavior.
#' @param font font for chart text; Segoe UI by default
#' @param font_size size for chart text; set to 12 by default
#' @export
#' @examples
#'
#' # all examples not run b/c of Travis CI failures
#' # failure due to not having Segoe UI font imported
#' # library(dplyr) # for %>% pipe
#' # # An N bar chart by default
#' # mtcars %>%
#' #   bar_chart_counts(var     = cyl,
#' #                    title   = "Number of mtcars by cylinder")
#' #
#' # # Use a grouping variable
#' # mtcars %>%
#' #   bar_chart_counts(var          = cyl,
#' #                    group_var    = vs,
#' #                    labels        = "pct",
#' #                    title        = "Percentage of V vs. Straight engines by # of cylinders")
#' #
#' # # Change default color
#' # mtcars %>%
#' #   bar_chart_counts(var       = cyl,
#' #                    var_color = "orange",
#' #                    title     = "Number of mtcars by cylinder")
#' # # Specify color by group
#' #   bar_chart_counts(mtcars, am, cyl,
#' #                    group_colors = c("orange", "green", "dark_blue"),
#' #                    labels = "pct")
bar_chart_counts <- function(df,
                             var,
                             group_var,
                             labels = "n",
                             var_color = "medium_blue",
                             group_colors,
                             title = NULL,
                             var_label,
                             digits = 1,
                             font = "Segoe UI",
                             font_size = 12) {
  # QC: Throw an error if object supplied to df is not a data.frame -----------
  if (!is.data.frame(df)) {
    stop("You must supply a data.frame to the df argument")
  }

  # QC: Throw an error if var was not specified  ------------------------------
  if (missing(var)) {
    stop("You must supply a column name to the var argument")
  }


  # Create a plot_data object -------------------------------------------------
  # plot_data should contain user specified column and its factor equivalent

  # Check if a grouping variable was specified
  if (missing(group_var)) {
    plot_data <- df %>%
      dplyr::select_(.dots = list(vec = lazyeval::lazy(var))) %>%
      dplyr::mutate(vec.factor = as.factor(vec)) %>%
      dplyr::group_by(vec.factor) %>%
      dplyr::tally() %>%
      dplyr::mutate(perc = n / sum(n))
  } else {
    plot_data <- df %>%
      dplyr::select_(.dots = list(
        vec = lazyeval::lazy(var),
        group.vec = lazyeval::lazy(group_var)
      )) %>%
      dplyr::mutate(
        vec.factor = as.factor(vec),
        group.factor = as.factor(group.vec)
      ) %>%
      dplyr::group_by(vec.factor, group.factor) %>%
      dplyr::tally() %>%
      dplyr::group_by(vec.factor) %>%
      dplyr::mutate(perc = n / sum(n)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(vec.factor, group.factor, fill = list(n = NA, perc = NA))
  }

  # Create a color palette ----------------------------------------------------

  # Check if group_var is supplied
  if (!missing(group_var)) {
    if (missing(group_colors)) {
      # QC: If group_var is supplied, but no colors, create a color palette
      #     while also making sure there are enough colors for each group

      # Select a color for each level of the factor-ed grouping variable. Must
      # be less than or equal to 11 (because we only have 11 TNTP colors)
      num_groups <- plot_data$group.factor %>%
        levels() %>%
        length()

      if (num_groups > 11) {
        stop("The maximum number of levels allowed in group_var is 11")
      }

      tntp_col_pal <- palette_tntp(
        "dark_blue", "medium_blue", "light_blue",
        "orange", "gold", "green", "dark_grey",
        "medium_grey", "light_grey", "white",
        "black"
      )[1:num_groups]
    } else {
      # QC: Throw an error if the number of levels in supplied group_var does
      # not equal the number of group_colors
      num_group_var <- plot_data$group.factor %>%
        levels() %>%
        length()
      num_group_col <- group_colors %>% length()

      if (num_group_var != num_group_col) {
        stop("The number of group_colors must equal
                                 the number of levels supplied to group_var")
      }
      # Switch color name strings to the HEX codes
      tntp_col_pal <- swap_colors(group_colors)
    }
  }

  # Check whether user specified an x axis label ------------------------------
  if (missing(var_label)) {
    var_label <- deparse(substitute(var))
  }

  # Build the N bar chart -----------------------------------------------------
  # Condition on presence of group_var

  if (missing(group_var)) {
    if (labels == "pct") {
      nbc <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = vec.factor, y = perc)) +
        ggplot2::geom_bar(fill = swap_colors(var_color), stat = "identity") +
        ggplot2::geom_text(ggplot2::aes(label = formattable::percent(janitor::round_half_up(perc, digits + 2), digits = digits)),
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
        ggplot2::geom_text(aes(label = formattable::percent(janitor::round_half_up(perc, digits + 2), digits = digits)),
          position = position_dodge(width = 0.9),
          vjust = -0.8,
          na.rm = TRUE,
          family = font,
          size = font_size * 0.35
        )
    } else {
      nbc <- ggplot2::ggplot(
        data = plot_data,
        mapping = aes(x = vec.factor, y = n, fill = group.factor)
      ) +
        ggplot2::geom_bar(position = "dodge", stat = "identity", na.rm = TRUE) + # silences warnings when there's an empty bar because of a subgroup of size 0
        ggplot2::scale_fill_manual(values = tntp_col_pal)


      nbc <- nbc +
        ggplot2::geom_text(ggplot2::aes(label = n),
          position = position_dodge(width = 0.9),
          vjust = -0.8,
          na.rm = TRUE,
          family = font,
          size = font_size * 0.35
        )
    }
  }

  # Polish the plot to presentation standards ---------------------------------

  # so labels don't get cropped, set the y scale 5% higher than the highest bar
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
    ggplot2::theme(
      axis.line.y = element_blank(),
      axis.line.x = element_line(
        color = "grey70",
        size = 0.20
      ),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        family = font,
        size = font_size
      ),
      axis.ticks = element_blank(),
      axis.title.x = element_text(
        family = font,
        size = font_size
      ),
      axis.title.y = element_blank(),
      legend.key = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(
        family = font,
        size = font_size
      ),
      legend.title = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        family = font,
        face = "bold",
        size = font_size
      )
    )
  nbc
}

# function to swap in custom TNTP colors

swap_colors <- function(x) {
  plyr::mapvalues(x,
    from = c(
      "dark_blue", "medium_blue",
      "light_blue", "green",
      "orange", "gold",
      "dark_grey", "dark_gray",
      "medium_grey", "medium_gray",
      "light_grey", "light_gray",
      "white", "black"
    ),
    to = c(
      "#034772", "#2888BC",
      "#73B7CE", "#699D46",
      "#EA8936", "#F9C347",
      "#58595B", "#58595B",
      "#7D7E81", "#7D7E81",
      "#C1C2C4", "#C1C2C4",
      "#FFFFFF", "#000000"
    ),
    warn_missing = FALSE
  )
}
