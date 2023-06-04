## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(tntpr)

# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(devtools, tidyverse, knitr)
if (!require("patchwork")) devtools::install_github("thomasp85/patchwork")

## ----knitr_options, include = FALSE-------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_chunk$set(error = TRUE)
knitr::opts_chunk$set(out.width='750px', dpi = 300)
knitr::opts_chunk$set(dev = "png", fig.width = 8, fig.height = 4.8889, dpi = 300)

## ----sample_datasets, include=FALSE-------------------------------------------

performance_data <- data.frame(teacher_experience = c(rep("0-3 years ", 5), rep("4-6 years", 4), rep("7+ years", 4)),
                      y1_teacher_performance = sample(0:100,size = 13,rep=TRUE),
                      y2_teacher_performance = sample(0:100,size = 13,rep=TRUE)) %>%
  mutate(y1_performance_quartile = ntile(x = y1_teacher_performance,n = 4))

survey_question <- data.frame(question = c(rep("To what extent do you \nagree with ...", 100)),
                              answer = sample(1:5, size = 100,rep=TRUE))

## ----theme_tntp, echo=FALSE, warning=FALSE------------------------------------

ex_plot_default <- performance_data %>%
  ggplot(aes(factor(teacher_experience))) +
  geom_bar() +
  labs(title = "default",
       subtitle = "Subtitle",
       x = "x label",
       y = "y label",
       caption = "caption")

ex_plot_theme_tntp <- performance_data %>%
  ggplot(aes(factor(teacher_experience))) +
  geom_bar() +
  labs(title = "theme_tntp()",
       subtitle = "Subtitle",
       x = "x label",
       y = "y label",
       caption = "caption") +
  theme_tntp()

ex_plot_theme_tntp_2018 <- performance_data %>%
  ggplot(aes(factor(teacher_experience))) +
  geom_bar() +
  labs(title = "theme_tntp_2018()",
       subtitle = "Subtitle",
       x = "x label",
       y = "y label",
       caption = "caption") +
  theme_tntp_2018()


## ---- echo=FALSE, warning=FALSE, fig.width=7----------------------------------
ex_plot_default

## ---- echo=FALSE, warning=FALSE, fig.width=7----------------------------------
ex_plot_theme_tntp

## ---- echo=FALSE, warning=FALSE, fig.width=7----------------------------------
ex_plot_theme_tntp_2018

## ----plot_color_palette, include = FALSE--------------------------------------

# This function is for displaying color palettes 
plot_color_palette <- function(dat, title = ""){
  dat %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    set_names(c("labels", "hex")) %>% 
    mutate(row = row_number()) %>% 
    ggplot(aes(x = row, y = 1, fill = hex)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label = paste0(labels, " (", hex, ")")), position = position_stack(vjust = 0.5), color = "white") + 
      scale_fill_identity() + 
      coord_flip() + 
      labs(title = title, 
           x = NULL, 
           y = NULL) + 
      theme_tntp_2018() + 
      theme(axis.text = element_blank(), 
            panel.grid = element_blank())
}


## -----------------------------------------------------------------------------
palette_tntp("dark_blue")

## ---- fig.height= 12, fig.width=5, warning=FALSE, echo = FALSE----------------
# A one-off function to plot color palettes, see code above if curious
plot_color_palette(tntpr::colors_tntp, title = "palette_tntp colors")

## ---- fig.height= 12, fig.width=7, warning=FALSE, echo = FALSE----------------

palettes <- c("default", "likert_4pt", "likert_5pt", "likert_6pt", "likert_orange_to_green_4pt", "likert_orange_to_green_5pt", "likert_orange_to_green_6pt")

palette_plots <- map(palettes, palette_tntp_scales) %>% 
  map2(palettes, plot_color_palette)

palette_plots[[1]] + (palette_plots[[2]] + palette_plots[[3]] + palette_plots[[4]]) + palette_plots[[5]] + palette_plots[[6]] + palette_plots[[7]] + plot_layout(ncol = 1)


## ---- fig.width=7, fig.length=5, warning=FALSE, echo=TRUE---------------------
performance_data %>%
  ggplot(aes(factor(teacher_experience), fill = factor(y1_performance_quartile))) +
    geom_bar(position = position_fill()) +
    labs(title = "Title",
         subtitle = "Subtitle",
         x = "x label",
         y = "y label",
         fill = "fill",
         caption = "caption") +
  theme_tntp_2018() +
  scale_fill_tntp()

## ---- fig.width=7, fig.length=5, warning=FALSE, echo=TRUE---------------------
performance_data %>% 
  ggplot(aes(x = y1_teacher_performance, y = y2_teacher_performance, color = factor(teacher_experience))) + 
    geom_point(size = 2) + 
    labs(title = "Title",
       subtitle = "Subtitle",
       x = "x label",
       y = "y label",
       fill = "fill",
       caption = "caption") + 
    theme_tntp_2018() + 
    scale_color_tntp()

## ---- fig.width=7, fig.length=5, warning=FALSE, echo = TRUE-------------------
survey_question %>%
  ggplot(aes(factor(question), fill = factor(answer))) +
  geom_bar(position = position_fill()) +
  labs(title = "Title",
       subtitle = "Subtitle",
       x = "x label",
       y = "y label",
       caption = "caption") +
  theme_tntp_2018() + 
  scale_fill_tntp(palette = "likert_5pt")

