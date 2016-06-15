#' @importFrom ggplot2 theme_minimal
ggplot2::theme_minimal

# Create TNTP color palette
# colors: [1]dark blue, [2]med blue, [3]light blue, [4]green, [5]orange, [6]gold, [7]dark grey, [8]med grey, [9]light grey
tntp.palette <- c("#034772","#2888BC","#73B7CE", "#699D46", "#EA8936", "#F9C347","#58595B", "#7D7E81", "#C1C2C4")

# USAGE:
# Add "+ scale_color_manual(values = tntp.palette)" to do colors w/ TNTP colors
# Add  "+ scale_fill_manual(values = tntp.palette)" to do fills w/ TNTP colors
