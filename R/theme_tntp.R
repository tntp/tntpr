# TITLE: theme_tntp.R
# AUTHOR(S): Alex Spurrier; Jake Russ
# DATE: Unknown
# UPDATED: 2016-06-15

# DESCRIPTION: TNTP ggplot2 theme
# USAGE:
# Add "+ scale_color_manual(values = tntp.palette)" to do colors w/ TNTP colors
# Add  "+ scale_fill_manual(values = tntp.palette)" to do fills w/ TNTP colors

theme_tntp <- list(theme_light() +
                     theme(text = element_text(family = "Segoe UI"),
                           legend.title = element_blank(), # remove variable name from legend
                           legend.position = "bottom",
                           legend.key = element_blank() # remove border from legend boxes
                     )
)
