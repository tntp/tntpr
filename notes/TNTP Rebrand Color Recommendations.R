# Load packages - first pacman, installing if necessary, then others
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, scales) # add more here as needed
pacman::p_load_current_gh("tntp/tntpr")

#
scales::show_col(c("#BC5A07", "#EA8835", "#FFB562", "#81D2EE", "#00A4c7", "#00355F"), ncol = 6)
scales::show_col(c("#BC5A07", "#EA8835", "#c1c2c4", "#00A4c7", "#00355F"), ncol = 5)
scales::show_col(c("#BC5A07", "#EA8835", "#00A4c7", "#00355F"), ncol = 4)

# New (2023) colors

# Primary
black       <- '#000000'
light_gray  <- '#F1F1EE'
light_green <- '#E2EDDC'
white       <- '#FFFFFF'

# Secondary
yellow      <- '#FDE57B'
light_red   <- '#FDDACA'
medium_blue <- '#00A5C7'
dark_red    <- '#C31F46'
dark_green  <- '#317D5C'

# Extended
dark_blue   <- '#00355F'
orange      <- '#F26C4C'
dark_gray   <- '#4A4A4A'
dark_yellow <- '#F2CF13'
light_blue  <- '#81D3EB'
medium_red  <- '#DA8988'
medium_green <- '#8FB09D'

# 4pt likert
scales::show_col(c(dark_red, light_red, light_green, dark_green), ncol = 4)

# 5pt likert
scales::show_col(c(dark_red, light_red, light_gray, light_green, dark_green), ncol = 5)

# 6pt likert
scales::show_col(c(dark_red, medium_red, light_red, light_green, medium_green, dark_green), ncol = 6)



# Example bar graph -------------------------------------------------------



county_data <- tntpr::fake_county |>
  filter(t_salary > 0)

school_salary <- county_data |>
  filter(t_salary != 0) |>
  group_by(school_year, school_name) |>
  summarize(avg_salary = mean(t_salary, na.rm = TRUE), .groups = "drop")

# create list of school names so we can easily filter data set for the number of schools we want
school_names <- unique(school_salary$school_name)

# only plot two schools
line_plot_schools <- school_salary |>
  filter(school_name %in% school_names[1:2])

bar_df <- school_salary |>
  filter(
    school_year == 2015,
    school_name %in% school_names[1:5]
  ) |>
  # add line breaks for better plotting
  mutate(school_name = str_wrap(school_name, 7))

ggplot(bar_df, aes(x = school_name, y = avg_salary)) +
  geom_bar(stat ="identity",
           position="identity",
           fill = if_else(bar_df$school_name == 'Acacia\nMiddle', tntp_colors('yellow'), tntp_colors('dark_green'))) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 5000)) +
  labs(
    title="Acacia had higher average salaries in 2015",
    subtitle = "Average teacher salaries in 2015 by school"
  ) +
  tntp_style(base_size = 16)

bar_df$school_name


# Likert ------------------------------------------------------------------

# the y-axis will contain text of an entire survey question
# we want to place line breaks in this text so plots look better
axis_line_breaks <- 40

# scales in HE questions, in order starting with the strongest
agree_disagree_scale <- rev(c("Strongly Agree", "Agree", "Somewhat Agree", "Somewhat Disagree", "Disagree", "Strongly Disagree"))

# put survey into long form and clean up question names
teacher_survey_he <- teacher_survey |>
  select(-timing) |>
  pivot_longer(cols = everything(), names_to = 'question', values_to = 'response')

# calculate percentage of responses to each high expectations question
teacher_survey_he_perc <- teacher_survey_he |>
  drop_na("response") |>
  # calculate the number of responses for each response option
  count(question, response, name = 'n_response') |>
  # calculate the number of responses for each question
  group_by(question) |>
  mutate(n_question = sum(n_response)) |>
  ungroup() |>
  # calculate percentages
  mutate(
    # calculate percentages
    percent = n_response / n_question,
    # make a column that is text of the percent for plotting
    percent_pretty = scales::percent(percent, accuracy = 1)
  )

# calculate percentage of strongly agree and agree
teacher_survey_he_perc <- teacher_survey_he_perc |>
  mutate(scale_strength = ifelse(response %in% !!agree_disagree_scale[c(5,6)], 'Strong response', 'Weak response')) |>
  group_by(question, scale_strength) |>
  mutate(strong_response_percent = sum(percent)) |>
  ungroup() |>
  mutate(
    strong_response_percent = ifelse(response == 'Agree', strong_response_percent, NA),
    # create line breaks for questions ,which will make plots look better
    question = str_wrap(question, axis_line_breaks),
    response = factor(response, levels = agree_disagree_scale)
  )

# colors to use
div_scale_colors <- tntp_palette('likert_6', reverse = TRUE)

# mapping of colors and responses for plot
div_color_pal <- div_scale_colors |>
  set_names(agree_disagree_scale)

legend_order <- c(agree_disagree_scale[c(1,2,3)], agree_disagree_scale[c(6,5,4)])

teacher_survey_div <- teacher_survey_he_perc |>
  mutate(
    perc_diverge = ifelse(str_detect(response, '[D|d]isagree'), percent * -1, percent),
    response = factor(response, levels = legend_order)
  )

ggplot(teacher_survey_div, aes(x = perc_diverge, y = question, fill = response)) +
  geom_col() +
  scale_fill_manual(
    values = div_color_pal, drop = FALSE,
    breaks = agree_disagree_scale,
    labels = agree_disagree_scale
  ) +
  geom_vline(aes(xintercept = 0), linetype = 1, linewidth = 1.2, alpha = .7) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, .25), labels = function(x) scales::percent(abs(x))) +
  labs(
    title = "High Expectations Survey Responses",
    x = NULL,
    y = NULL
  ) +
  tntp_style(base_size = 16)


# Multiple Lines ----------------------------------------------------------


#Prepare data
school_salary <- county_data |>
  filter(t_salary != 0) |>
  group_by(school_year, school_name) |>
  summarize(avg_salary = mean(t_salary, na.rm = TRUE), .groups = "drop")

# create list of school names so we can easily filter data set for the number of schools we want
school_names <- unique(school_salary$school_name)

# only plot two schools
line_plot_schools <- school_salary |>
  filter(school_name %in% school_names[1:4])

line_colors <- tntp_colors('dark_green', 'medium_blue', 'dark_red', 'yellow')

ggplot(line_plot_schools, aes(x = school_year, y = avg_salary, color = school_name)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 5000)) +
  scale_colour_manual(values = line_colors) +
  labs(
    title="Average Teacher Salaries",
    subtitle = "Relatively constant from 2012 to 2015",
    x = NULL,
    y = 'Average teacher salary'
  ) +
  tntp_style() +
  guides(color = guide_legend(nrow = 2))

