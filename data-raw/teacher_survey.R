# code to prepare `teacher_survey` dataset goes here
# fake dataset only includes high expectations questions

# high expectations questions
he_questions <- c(
  "It's fair to expect students in this class to master these standards by the end of the year.",
  "One year is enough time for students in this class to master these standards.",
  "All students in my class can master the grade-level standards by the end of the year.",
  "The standards are appropriate for the students in this class."
)

scales <- c("Strongly Agree", "Agree", "Somewhat Agree", "Somewhat Disagree", "Disagree", "Strongly Disagree")

n <- 20

teacher_survey <- data.frame(matrix(nrow = n, ncol = length(he_questions)))
colnames(teacher_survey) <- he_questions

for (col_name in he_questions) {
  teacher_survey[[col_name]] <- sample(scales, n, replace = TRUE)
}

teacher_survey$timing <- c(rep("Pre", 10), rep("Post", 10))

teacher_survey <- dplyr::select(teacher_survey, timing, dplyr::everything())

usethis::use_data(teacher_survey, overwrite = TRUE)
