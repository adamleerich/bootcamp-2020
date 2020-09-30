# knitr::opts_chunk$set(
#     warning = TRUE
#   , error = TRUE
#   , echo = TRUE
#   , message = FALSE
#   , fig.height = 4.5
#   , fig.pos = "t"
#   , collapse = TRUE
# )
# 
# knitr::opts_knit$set(
#   root.dir = '../'
# )


knitr::opts_chunk$set(
  # comment = '  ',
  echo = TRUE,
  fig.width = 4.5,
  fig.height = 4.5,
  fig.cap = NULL,
  dpi = 600,
  # class.output = 'plaintext',
  warning = TRUE,
  error = TRUE,
  echo = TRUE,
  message = FALSE,
  fig.pos = "t",
  collapse = TRUE)

library <- function(...) {
  suppressPackageStartupMessages(
    suppressWarnings(
      base::library(...)))}

require <- function(...) {
  stop("DO NOT USE require() WHILE TEACHING!")}



