# TITLE: theme_tntp.R
# AUTHOR(S): Alex Spurrier
# DATE: Unknown
# UPDATED:

# DESCRIPTION: TNTP ggplot2 theme

theme_tntp <- list(theme_light() +
                     theme(text = element_text(family = "Segoe UI"),
                           legend.title = element_blank(), # remove variable name from legend
                           legend.position = "bottom",
                           legend.key = element_blank() # remove border from legend boxes
                     )
)

# add + scale_color_manual(values = tntp.palette) to do colors w/ TNTP colors
# + scale_fill_manual(values = tntp.palette) to do fills w/ TNTP colors
