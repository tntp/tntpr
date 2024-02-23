#' @title Set the formatting options for a TNTP Data Memo
#'
#' @description
#' internal function that calls standard formatting options for the Data Memo RMarkdown template
#' moved here to keep the actual memo template cleaner and easier to use
#'
#' @returns nothing
#' @export
#' @examples
#' # not run: set_data_memo_formatting()
#'
set_data_memo_formatting <- function() {
  # set knitr options
  knitr::opts_chunk$set(dev = "png", fig.width = 6, fig.height = 3.667, dpi = 300) # Sam uses 9" (H) x 5.5" (W) for PPT insertions but trying smaller here for Word
  knitr::opts_chunk$set(echo = FALSE)
  knitr::opts_chunk$set(size = "small", background = "white")
  knitr::opts_chunk$set(fig.path = "figures/fig-")
  knitr::opts_chunk$set(highlight = TRUE, comment = NA, tidy = FALSE)
  knitr::opts_chunk$set(knitr.table.format = "html")
}

#' @title Create sequential figure numbers
#'
#' @description
#' Create sequential figure numbers
#'
#' @param x character string description of the figure
#' @returns nothing
#' @export
#' @examples
#' # not run, in RMarkdown doc: `r figureN("Distribution of cars by cylinder count")`
#'
#' #
figureN <- local({
  i <- 0
  function(x) {
    i <<- i + 1
    paste("Figure ", i, ". ", x, sep = "")
  }
})


#' @title Create sequential table numbers
#'
#' @description
#' Create sequential table numbers
#'
#' @param x character string description of the figure
#'
#' @returns nothing
#' @export
#' @examples
#' # not run, in RMarkdown doc: `r tableN("Distribution of cars by cylinder count")`
tableN <- local({
  i <- 0
  function(x) {
    i <<- i + 1
    paste("Table ", i, ". ", x, sep = "")
  }
})
