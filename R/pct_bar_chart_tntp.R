
# bc <- ggplot(data = plot_data, aes(x = var.factor)) +
#   geom_bar(mapping = aes(y     = (..count..) / sum(..count..),
#                          fill  = var.factor),
#            stat    = "count") +
#   geom_text(mapping = aes(y     = ((..count..) / sum(..count..)) + 0.02,
#                           label = scales::percent((..count..) / sum(..count..) )),
#             stat    = "count",
#             vjust   = -0.10) +
#   scale_y_continuous(labels = scales::percent)
