source('common.R')

knitr::include_graphics('images/options_general.png')

knitr::include_graphics('./images/options_appearance.png')

knitr::include_graphics('./images/options_pane_layout.png')

2 + 5
-2 ^ 0.5 # which operator takes precedence
sqrt(2)
abs(-15.4)
3 > 4 # logical

my_name <- "Brian"
my_name
my_age <- 48
my_age

exp(sqrt(pi))
library(magrittr)

pi %>% 
  sqrt() %>% 
  exp()

## ?plot
## ??cluster

getwd()

knitr::include_graphics('images/working_directory.png')

knitr::include_graphics('images/project_directory.png')

.libPaths()
