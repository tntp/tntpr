#' @title Collapse factor levels into three groups: top-n levels, bottom-n levels, and all other levels.
#'
#' @description
#' Typical usage is for say, a Likert question on a survey where we group respondents into strongly disagree/disagree, somewhat disagree/somewhat agree, and agree/strongly agree.
#'
#' @param input_vec the factor variable to lump.
#' @param n number of levels to include in top and bottom groups
#' @return An ordered factor.
#' @export
#' @examples
#' fct_lump_ends(as.factor(mtcars$hp), 3)


fct_lump_ends <- function(input_vec, n = 2) {

  # Initial type error catching
  if (!is.factor(input_vec)) {
    stop("factor_vec is not of type 'factor'")
  }

  num_levels_in_var <- nlevels(input_vec)

  # handle bad inputs
  if (!num_levels_in_var > 2) {
    stop("input factor variable must have at least 3 levels")
  }
  if (num_levels_in_var < 2 * n) {
    stop(paste0("there are ", num_levels_in_var, " levels in the variable and ", n, " levels in each of the top and bottom groups.\nSince 2 * ", n, " = ", 2 * n, " is greater than ", num_levels_in_var, ", there would be overlap in the top and bottom groups and some records will be double-counted."))
  }
  if (n < 1 || n %% 1 != 0) {
    stop("n must be a whole number at least 1")
  }

  var_name <- deparse(substitute(input_vec))

  # Identify top/mid/bottom group labels for printing
  groups <- get_level_groups(input_vec, n, num_levels_in_var)

  # convert input vector into grouped variable
  new_vec <- ifelse(as.numeric(input_vec) <= n,
                    groups$top,
                    ifelse(as.numeric(input_vec) > (num_levels_in_var - n),
                           groups$bot,
                           groups$mid
                    )
  )

  # recode variable as hi-med-lo factor so table prints w/ correct sorting
  if (!is.na(groups$mid)) {
    new_vec <- factor(new_vec, levels = c(groups$top, groups$mid, groups$bot), ordered = TRUE)
  } else {
    new_vec <- factor(new_vec, levels = c(groups$top, groups$bot), ordered = TRUE)
  }

new_vec
}


# Return groupings for a factor variable in the fct_lump_ends() function

get_level_groups <- function(vec, n, num_levels_in_var) {
  top_n_lvls <- paste(levels(vec)[1:n], collapse = ", ")
  bot_n_lvls <- paste(levels(vec)[(num_levels_in_var - n + 1):num_levels_in_var], collapse = ", ")

  # Identify middle combinations, if needed
  if (num_levels_in_var > 2 * n) {
    mid_lvls <- paste(levels(vec)[(n + 1):(num_levels_in_var - n)], collapse = ", ")
  } else {
    mid_lvls <- NA
  }

  # Truncate strings if needed
  ## Middle groups are variable size, so displaying the N there is useful;
  ## Top/Bottom are user-specified size, so just truncate the labels
  if (!is.na(mid_lvls) & nchar(mid_lvls) > 30) {
    mid_lvls <- paste0("<<< Middle Group (", num_levels_in_var - 2 * n, " categories) >>>")
  }
  if (nchar(top_n_lvls) > 30) {
    top_n_lvls <- paste0(substr(top_n_lvls, 1, 27), "...")
  }
  if (nchar(bot_n_lvls) > 30) {
    bot_n_lvls <- paste0(substr(bot_n_lvls, 1, 27), "...")
  }

  list(top = top_n_lvls, mid = mid_lvls, bot = bot_n_lvls)
}
