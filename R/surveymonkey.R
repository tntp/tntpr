library(jsonlite)

#' Get the list of surveys from a SurveyMonkey account
#' 
#' @description Returns information about the surveys in a SurveyMonkey account.
#'
#' @param auth_token Get this from your SurveyMonkey app
#' @param page Which page of results to display?
#' @param per_page How many results to display?
#'
#' @return a list with information about the surveys in the account.  You probably want to access $data, the data.frame with survey IDs, for use in other functions.
#' @export
#'
#' @examples
sm_get_surveys <- function(auth_token, page = 1, per_page = 250) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your auth token for SurveyMonkey: ')
  }
  
  
  auth <- paste("bearer", auth_token, sep=" ");
  
  url <- paste('https://api.surveymonkey.net/v3/surveys?page=', page, '&per_page=', per_page, sep='')
  
  survey_list_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_list_response$status_code != 200) {
    stop(paste('Bad response from server: ', httr::http_status(survey_list_response)))
  }
  
  json <- httr::content(survey_list_response, as = 'text')
  survey_list <- jsonlite::fromJSON(json)
  survey_list
}


#' Get information about respondents to a survey
#'
#' @description Call on a single survey ID retrieved using \code{sm_get_surveys()}, to get a list with information about the respondents to a particular survey.
#' @param auth_token Get this from your SurveyMonkey app
#' @param survey_id The ID of the survey you want information about; use \code{sm_get_survey()} to obtain this.
#' @param page Which page of results to to display?
#' @param per_page How many results to display?
#'
#' @return
#' @export
#'
#' @examples
sm_get_responses <- function(auth_token, survey_id, page = 1, per_page = 250) {
  if (missing(auth_token)) {
    auth_token <- readline('Enter your auth token for SurveyMonkey: ')
  }
  if (missing(survey_id)) {
    stop('Survey ID is required')
  }
  
  auth <- paste("bearer", auth_token, sep=" ");
  url <- paste('https://api.surveymonkey.net/v3/surveys/', survey_id, '/responses/bulk', sep='')
  
  survey_responses_response <- httr::GET(url=url, add_headers("Content-Type" = "application/json", "Authorization" = auth ))
  
  if (survey_responses_response$status_code != 200) {
    stop(c('Bad response from server: ', httr::http_status(survey_responses_response)))
  }
  
  json <- httr::content(survey_responses_response, as = 'text')
  survey_responses <- jsonlite::fromJSON(json)
  
  survey_responses
}

