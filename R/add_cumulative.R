add_cumulative <- function(df, colname, dir = "down"){

  if(!missing(colname)){
    colname <- rlang::enquo(colname)
  } else if("valid_percent" %in% names(df)) {
  colname <- rlang::sym("valid_percent")
  } else if("percent" %in% names(df)){
    colname <- rlang::sym("percent")
  } else {
    stop("\"colname\" not specified and default columns valid_percent and percent are not present in data.frame df")
  }

  target <- pull(df, !! colname)

  if(dir == "up"){
    target <- rev(target)
  }
  df$cumulative <- cumsum(ifelse(is.na(target), 0, target)) + target*0 # an na.rm version of cumsum, from https://stackoverflow.com/a/25576972
  if(dir == "up"){
    df$cumulative <- rev(df$cumulative)
  }
  df
}

library(dplyr)
mtcars %>%
  add_cumulative(mpg)

library(janitor)
mtcars %>%
  tabyl(cyl) %>%
  add_cumulative()

c(0, 1, 2, 3, 3, 3, NA) %>%
  tabyl() %>%
  add_cumulative()

c(0, 1, 2, 3, 3, 3, NA) %>%
  tabyl() %>%
  add_cumulative(dir = "up")
