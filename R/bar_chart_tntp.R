#' Bar chart with TNTP polish
#'
#' Takes a user supplied data frame and turns the designated column into
#' an N bar chart (uses position dodge from ggplot2).
#'@rdname bar_chart_tntp
#'@param df the data.frame to be used in the bar chart
#'@param var unquoted column name for desired variable
#'@param group_var unquoted column name for group variable
#'@param group_colors character vector of group colors
#'@param title main chart title
#'@param font font for chart text; Segoe UI by default
#'@param font_size size for chart text; set to 12 by default
#'@param var_color color for non-grouped charts; set to light_grey by default
#'@param ... additional arguments
#'@export
#'@examples
#'
#'# An  bar chart by default
#'mtcars %>%
#'  bar_chart_tntp(var     = cyl,
#'                 title   = "Number of mtcars by cylinder")
#'
#'# With a grouping variable
#'mtcars %>%
#'  bar_chart_tntp(var          = cyl,
#'                 group_var    = vs,
#'                 title        = "Percentage of mtcars by cylinder")

bar_chart_tntp <- function(df           = NULL,
                           var,
                           group_var,
                           group_colors,
                           title        = NULL,
                           font         = "Segoe UI",
                           font_size    = 12,
                           var_color    = palette_tntp("dark_blue"), ...) {

  # QC: Throw an error if object supplied to df is not a data.frame -----------
  testthat::expect(exp = is.data.frame(df),
                   "You must supply a data.frame to the df argument")

  # QC: Throw an error if var was not specified  ------------------------------
  testthat::expect(exp = !missing(var),
                   "You must supply a column name to the var argument")

  # Create a plot_data object -------------------------------------------------
  # plot_data should contain user specified column and its factor equivalent

  # Check if a grouping variable was specified
  if(missing(group_var)){

    plot_data <- df %>%
      dplyr::select_(.dots = list(vec = lazyeval::lazy(var))) %>%
      dplyr::as_data_frame() %>%
      dplyr::mutate(vec.factor = as.factor(vec))

  } else {

    plot_data <- df %>%
      dplyr::select_(.dots = list(vec       = lazyeval::lazy(var),
                                  group.vec = lazyeval::lazy(group_var))) %>%
      dplyr::as_data_frame() %>%
      dplyr::mutate(vec.factor   = as.factor(vec),
                    group.factor = as.factor(group.vec))
  }
  # Create a color palette ----------------------------------------------------

  # Check if group_var is supplied
  if(!missing(group_var)){

    if(missing(group_colors)) {

      # QC: If group_var is supplied, but no colors, create a color palette
      #     while also making sure there are enough colors for each group

      # Select a color for each level of the factor-ed grouping variable. Must
      # be less than or equal to 11 (because we only have 11 TNTP colors)
      num_groups <- plot_data$group.factor %>% levels() %>% length()

      testthat::expect_lte(num_groups, 11,
                           "The maximum number of levels allowed in group_var is 11")

      tntp_col_pal <- palette_tntp("dark_blue", "medium_blue", "light_blue",
                                   "orange", "gold", "green", "dark_grey",
                                   "medium_grey", "light_grey", "white",
                                   "black")[1:num_groups]

    } else {

      # QC: Throw an error if the number of levels in supplied group_var does
      # not equal the number of group_colors
      num_group_var <- plot_data$group.factor %>% levels() %>% length()
      num_group_col <- group_colors %>% length()

      testthat::expect_identical(num_group_var, num_group_col,
                                 info = "The number of group_colors must equal
                                 the number of levels supplied to group_var")

      tntp_col_pal <- group_colors %>%
        # Switch color name strings to the HEX codes
        plyr::mapvalues(from         = c("dark_blue",   "medium_blue",
                                         "light_blue",  "orange",
                                         "gold",        "green",
                                         "dark_grey",   "dark_gray",
                                         "medium_grey", "medium_gray",
                                         "light_grey",  "light_gray",
                                         "white",       "black"),

                        to           = c("#034772", "#2888BC",
                                         "#73B7CE", "#699D46",
                                         "#EA8936", "#F9C347",
                                         "#58595B", "#58595B",
                                         "#7D7E81", "#7D7E81",
                                         "#C1C2C4", "#C1C2C4",
                                         "#FFFFFF", "#000000"),
                        warn_missing = FALSE)
    }
  }

  # Build the N bar chart -----------------------------------------------------
  # Condition on presence of group_var

  if(missing(group_var)){

    nbc <- ggplot(data = plot_data, aes(x = vec.factor)) +
      geom_bar(fill = var_color) +
      geom_text(mapping  = aes(label = ..count.., y = (..count..)),
                stat     = "count",
                vjust    = -0.8)
  } else {

    nbc <- ggplot(data    = plot_data,
                  mapping = aes(x = vec.factor, fill = group.factor)) +
      geom_bar(position = "dodge") +
      geom_text(mapping  = aes(label = ..count.., y = (..count..)),
                position = position_dodge(width = 1),
                stat     = "count",
                vjust    = -0.8) +
      scale_fill_manual(values = tntp_col_pal)

  }

  # Polish the plot to presentation standards ---------------------------------

  nbc <- nbc +
    scale_y_continuous(expand = c(0, 0.6)) +
    ggtitle(title) +
    theme(axis.line.y      = element_blank(),
          axis.line.x      = element_line(color = "grey70",
                                          size  = 0.20),
          axis.text.y      = element_blank(),
          axis.text.x      = element_text(family = font,
                                          size   = font_size),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),

          legend.key       = element_blank(),
          legend.position  = "bottom",
          legend.text      = element_text(family = font,
                                          size   = font_size),
          legend.title     = element_blank(),

          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),

          plot.title       = element_text(family = font,
                                          face   = "bold",
                                          size   = font_size))
    nbc
}
