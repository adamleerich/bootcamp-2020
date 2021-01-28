# Get a list of all possible distribution functions in base R and actuar package


library(actuar)
library(MASS)
library(fitdistrplus)
rm(list = ls(all = TRUE))


pkgs <- search()
objs <- NULL
for (i in 2:length(pkgs)) {
  pkg <- pkgs[i]
  pobjs <- ls(pkg)
  if (length(pobjs) > 0) {
    dfobjs <- data.frame(package = pkg, object = pobjs)
    if (is.null(objs)) {
      objs <- dfobjs
    } else {
      objs <- rbind(objs, dfobjs)
    }
  }
}

objs <- as_tibble(objs)


drpq <- objs[grepl('^[drpq].*', objs$object), ]
drpq$basename <- gsub('^.(.*)$', '\\1', drpq$object)


basenames <- unique(drpq$basename)

out <- character()
for (i in 1:length(basenames)) {
  
  if (sum(paste0(c('r', 'd', 'p', 'q'), basenames[i]) %in% drpq$object) == 4) {
    out <- c(out, basenames[i])
  }
  
}


dists <- drpq %>% 
  filter(drpq$basename %in% out)

dists %>% 
  group_by(package, basename) %>% 
  summarize() %>% 
  clipr::write_clip()





