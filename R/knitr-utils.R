#' @title Set the formatting options for a TNTP Data Memo
#'
#'
#' @description
#' internal function that calls standard formatting options for the Data Memo RMarkdown template
#' moved here to keep the actual memo template cleaner and easier to use
#'
#' @return nothing
#' @export
#' @examples
#' # not run: set_data_memo_formatting()
#'
set_data_memo_formatting <- function(){
  # set knitr options
  library(knitr)
  opts_chunk$set(dev = "png", fig.width = 9, fig.height = 5.5, dpi = 300) # Sam uses these sizes in PPT slides but it's not scientific and maybe not ideal for Word
  opts_chunk$set(echo = FALSE)
  opts_chunk$set(size = "small", background = "white")
  opts_chunk$set(fig.path = "figures/fig-")
  opts_chunk$set(highlight = TRUE, comment = NA, tidy = FALSE)
  opts_chunk$set(knitr.table.format = "html")
}

#' @title Create sequential figure numbers
#'
#'
#' @description
#' Create sequential figure numbers
#'
#' @return nothing
#' @export
#' @examples
#' # not run, in RMarkdown doc: `r figureN("Distribution of cars by cylinder count")`
#'
#
figureN <- local({
  i = 0
  function(x) {
    i <<- i + 1
    paste('Figure ', i, '. ', x, sep = '')
  }
})


#' @title Create sequential table numbers
#'
#'
#' @description
#' Create sequential table numbers
#'
#' @return nothing
#' @export
#' @examples
#' # not run, in RMarkdown doc: `r tableN("Distribution of cars by cylinder count")`
tableN <- local({
  i = 0
  function(x) {
    i <<- i + 1
    paste('Table ', i, '. ', x, sep = '')
  }
})
