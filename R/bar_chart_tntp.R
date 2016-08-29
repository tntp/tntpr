#' Bar chart with TNTP polish
#'
#' Takes a user supplied data frame and turns the designated column into
#' a bar chart. Can specify an "n" bar chart or a "percent" bar chart.
#'@rdname bar_chart_tntp
#'@param df the data.frame to be used in the bar chart
#'@param var unquoted column name for desired variable
#'@param display set to "n" by default; will also accept "percent"
#'@param title main chart title
#'@param ... additional arguments
#'@export
#'@examples
#'
#'# An N bar chart by default
#'mtcars %>%
#'  bar_chart_tntp(var     = cyl,
#'                 title   = "Number of mtcars by cylinder")
#'
#'#'mtcars %>%
#'  bar_chart_tntp(var     = cyl,
#'                 title   = "Percentage of mtcars by cylinder",
#'                 display = "percent")

bar_chart_tntp <- function(df = NULL, var = NULL, display = "n", title = NULL, ...){

  # Throw an error if df is not a data.frame
  testthat::expect(exp = is.data.frame(df),
                   "You must supply a data.frame to bar_chart_tntp")
  testthat::expect(exp = display %in% c("n", "percent"),
                   "display only accepts 'n' or 'percent' please check spelling")

  # Use dplyr to make a 2 column dataframe.
  # (1) The original user specified column
  # (2) A factor version of the original

  plot_data <- dplyr::select_(df, var = deparse(substitute(var))) %>%
    dplyr::as_data_frame() %>%
    dplyr::mutate(var.factor = as.factor(var))

  # Select a colour for each level of the factor-ed variable
  # Must be less than 9 (because we only have 9 TNTP colors)
  num_groups <- plot_data$var.factor %>% levels() %>% length()

  testthat::expect_lte(num_groups, 9)

  tntp_col_pal <- palette_tntp("dark_blue", "medium_blue", "light_blue",
                               "orange", "gold", "green", "dark_grey",
                               "medium_grey", "light_grey")[1:num_groups]

  # Start by building either an N bar chart or a percent bar chart
  # and then polish it
  if(display %in% "n") {

  bc <- ggplot(data = plot_data, aes(x = var.factor)) +
          geom_bar(aes(fill = var.factor)) +
          geom_text(mapping = aes(label = ..count.., y = (..count.. + 1)),
                    stat    = "count",
                    vjust   = -1)
  } else { #
    bc <- ggplot(data = plot_data, aes(x = var.factor)) +
            geom_bar(mapping = aes(y     = (..count..) / sum(..count..),
                                   fill  = var.factor),
                     stat    = "count") +
            geom_text(mapping = aes(y     = ((..count..) / sum(..count..)) + 0.02,
                                    label = scales::percent((..count..) / sum(..count..) )),
                      stat    = "count",
                      vjust   = -0.10) +
            scale_y_continuous(labels = scales::percent)

    }

    # Polish the plot to presentation standards
  bc <- bc +
    scale_fill_manual(values = tntp_col_pal) +
    ggtitle(title) +
    theme(axis.line.y      = element_blank(),
          axis.line.x      = element_line(color = "grey70",
                                          size  = 0.20),
          axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),

          legend.key       = element_blank(),
          legend.position  = "bottom",
          legend.text      = element_text(family = "Segoe UI",
                                                   size   = 12),
          legend.title     = element_blank(),

          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),

          plot.title       = element_text(family = "Segoe UI",
                                          face   = "bold",
                                          size   = 12))
    bc
}
