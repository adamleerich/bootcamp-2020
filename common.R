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




info <- function(x, ...) {
  UseMethod("info")
}

# [ALR Nov 19, 2020]
# Edited to show data type and some info on non-numeric columns
info.data.frame <- function(dataframe) {
  
  out <- NULL
  for (i in 1:ncol(dataframe)) {
    
    v <- dataframe[, i, drop = TRUE]
    num <- is.numeric(v) | is.logical(v)
    lv <- length(v)
    vn <- v[!is.na(v)]
    uniques <- length(unique(vn))
    typ <- class(v)
    
    if (num) {
      qv <- stats::quantile(
        vn, 
        probs = c(0, 0.25, 0.5, 0.75, 1))
      sdv <- sd(vn)
      muv <- mean(vn)
      zero_count = sum(vn == 0)
      lvls <- NA
    } else {
      qv <- rep(NA, 5)
      sdv <- NA
      muv <- NA
      zero_count = NA
      lvls <- table(v) %>% sort(decreasing = TRUE) %>% 
        names %>% `[`(., 1:min(10, length(.))) %>% paste0(collapse = ', ')
    }
    
    qvr <- data.frame(
      row.names = i,
      column = names(dataframe)[i],
      type = typ,
      min = qv[1],
      q25 = qv[2],
      median = qv[3],
      mean = muv,
      q75 = qv[4],
      max = qv[5],
      sd = sdv,
      zero_count,
      # sd2ratio = sum(abs(vn - muv) < 2 * sdv) / length(vn),
      na_count = lv - length(vn),
      unique_values = uniques,
      levels = lvls)
    
    if (is.null(out)) { 
      out <- qvr } else { out <- rbind(out, qvr) }  
    
  }
  return(out)
}



