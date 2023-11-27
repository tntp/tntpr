#' Fake student data from the Wisconsin State Dept. of Ed
#'
#' A generated data set containing data on 1200 imaginary individual K-12 students in Wisconsin.  They are nested within 6 schools in 3 districts.  In adapting this from the source, Sam switched the school and district variables (there had been multiple districts per school) and made other minor changes, including dropping columns that I didn't understand or that didn't seem relevant (e.g., variables like "luck" that were used to calculate the reading and math scores).
#'
#' @format A data frame with 2700 rows and 26 variables:
#' \describe{
#'   \item{student_id}{numeric: student's unique ID #}
#'   \item{grade}{numeric: grade level}
#'   \item{district}{numeric: district code}
#'   \item{school}{numeric: school code}
#'   \item{white}{numeric: is the student white?}
#'   \item{black}{numeric: is the student black?}
#'   \item{hisp}{numeric: is the student Hispanic?}
#'   \item{indian}{numeric: is the student Native-American Indian?}
#'   \item{asian}{numeric: is the student Asian?}
#'   \item{econ}{numeric: is the student economically-disadvantaged?}
#'   \item{female}{numeric: is the student female?}
#'   \item{ell}{numeric: is the student an English Language Learner?}
#'   \item{disab}{numeric: does the student have a learning disability?}
#'   \item{year}{numeric: school year}
#'   \item{attday}{numeric: days attended}
#'   \item{readSS}{numeric: student's reading standardized test score}
#'   \item{mathSS}{numeric: student's math standardized test score}
#'   \item{proflvl}{factor: student's proficiency level}
#'   \item{race}{factor: student's single-category race}
#'   ...
#' }
#' @source \url{https://github.com/jknowles/r_tutorial_ed/}, posted under a Creative Commons license. The script used to generate the data set is here, although not very well documented: \url{https://github.com/jknowles/r_tutorial_ed/blob/master/data/simulate_data.R}
"wisc"

#' Fake teacher roster dataset from OpenSDP
#'
#' The Fake County synthetic panel dataset contains approximately 40,000 records comprising four years of data with roughly 10,000 teachers per year. The dataset includes information about teacher demographics, teaching assignments, salary, credentials, experience, evaluation scores, and hiring and retention status. It also includes information about school types and average student characteristics for each school. There are no real teachers in the dataset, but it is based on real data. Fake County was developed as an offshoot of the Strategic Data Project's work on human capital diagnostics for school districts and state education departments, and can be used for teaching or collaboration. The data was synthesized using the R synthpop package.
#'
#' @format A data frame with 39,339 rows and 38 variables:
#' \describe{
#'   \item{tid}{double: Teacher ID}
#'   \item{fake_data}{double: Record Is Simulated}
#'   \item{school_year}{double: School Year}
#'   \item{school_code}{double: School Code}
#'   \item{school_name}{character: School Name}
#'   \item{t_male}{double: Teacher Is Male}
#'   \item{t_race_ethnicity}{double: Teacher Race/Ethnicity}
#'   \item{t_job_area}{double: Teacher Assignment Type}
#'   \item{t_salary}{double: Monthly Salary}
#'   \item{t_nbpts}{double: Teacher Has National Board Certification}
#'   \item{t_tenured}{double: Teacher Is Tenured}
#'   \item{t_experience}{double: Years of Teaching Experience}
#'   \item{t_fte}{double: Teacher's FTE Status}
#'   \item{t_highest_degree}{double: Teacher's Highest Degree}
#'   \item{t_licensed_stem}{double: Teacher Is Licensed In STEM Field}
#'   \item{t_eval_obs}{double: Evaluation Summary Observation Score}
#'   \item{t_eval_growth}{double: Evaluation Summary Student Growth Score}
#'   \item{t_stay}{double: Teacher in Same School in Following Year}
#'   \item{t_transfer}{double: Teacher in Different School in Following Year}
#'   \item{t_leave}{double: Teacher Not Teaching in Fake County Schools in Following Year}
#'   \item{t_novice}{double: Teacher Is Novice First-Year Teacher}
#'   \item{t_new_hire}{double: Teacher Did Not Teach in Fake County in Prior Year}
#'   \item{sch_elem}{double: School Is Elementary School}
#'   \item{sch_middle}{double: School Is Middle School}
#'   \item{sch_high}{double: School Is High School}
#'   \item{sch_alternative}{double: School Is Alternative School}
#'   \item{sch_regular}{double: School Is Regular School}
#'   \item{sch_title_1}{double: School Is Title 1 School}
#'   \item{sch_magnet}{double: School Is Magnet School}
#'   \item{sch_vocati~l}{double: School is Vocational School}
#'   \item{sch_region}{double: School Region Code}
#'   \item{sch_calendar_type}{double: School Calendar Type}
#'   \item{sch_iep_pct}{double: School Special Education Student Share in 2012-15}
#'   \item{sch_minority_pct}{double: School Minority Student Share in 2012-15}
#'   \item{sch_frpl_pct}{double: School Free and Reduced Price Lunch Student Share in 2012-15}
#'   \item{sch_ela_avg}{double: School ELA Test Score Average in 2012-15 (in standard deviations)}
#'   \item{sch_math_avg}{double: School Math Test Score Average in 2012-15 (in standard deviations)}
#'   \item{sch_enrollment_2015}{double: School Enrollment in 2015}
#' }
#' @source \url{https://github.com/OpenSDP/fake-county}, posted under a Creative Commons license.
"fake_county"

#' Teacher survey data
#'
#' Simulated teacher survey data. Data only includes teh four TNTP high expectations questions.
#'
#' @format ## `teacher survey`
#' A data frame with 5 columns and 20 rows. The five columns are a `timing` column,
#' followed by four column for each of the four high expectations questions. Responses
#' are on the 'strongly agree' to strongly disagree' 6-point scale.
#'
#' @source simulated in `data-raw/teacher_survey.R`
"teacher_survey"
