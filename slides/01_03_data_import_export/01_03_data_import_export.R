source('common.R')

knitr::include_graphics('images/working_directory.png')

my_file <- file.path('data', 'read_excel.xlsx')
my_dir <- file.path('regions', 'north')
my_file
my_dir

file.exists(my_file_name)
dir.exists(my_dir)

dir.create(my_dir)
dir.create(my_dir, recursive = TRUE)
dir.exists(my_dir)

file.copy(my_file, my_dir)

unlink(my_dir, recursive = TRUE)
unlink('regions')
unlink('regions', recursive = TRUE)

library(magrittr)
tibble::tibble(
  region = c('North', 'South', "East", "West")
  , premium = c(100, 550, 300, 700)
) %>% 
  knitr::kable()

library(tidyverse)

tbl_insurance_data <- tibble(
  region = c('North', 'South', "East", "West")
  , premium = c(100, 550, 300, 700)
)

tbl_insurance_data %>% 
  write_csv(file.path("data", "test.csv"))

tbl_read_in <- read_csv(file.path("data", "test.csv"))

library(randomNames)

num_policies <- 10e3
regions <- c('North', 'South', "East", "West") %>% 
  as.factor()

set.seed(1234)
tbl_policy <- tibble(
  policyholder = randomNames(num_policies)
  , effective_date = as.Date('2000-01-01') + runif(num_policies, 0, 365 * 10)
  , premium = rnorm(num_policies, mean = 10e3, sd = 2e3)
  , num_employees = sample(seq_len(4e3), num_policies, replace = TRUE)
  , region = sample(regions, num_policies, replace = TRUE)
  , new_v_renewal = sample(c(TRUE, FALSE), num_policies, replace = TRUE)
)

tbl_policy %>% 
  write_csv(file.path('data', 'policy.csv'))

tbl_nothing <- read_csv('policy.csv')

tbl_policy_in <- read_csv(file.path("data", "policy.csv"))

guess_parser(c('1', '2'))
guess_parser(c('1', '2.0'))
guess_parser(c('2012-01-13', '2014-02-16'))
guess_parser(c('1/13/2012', '2014/2/2016'))
guess_parser(c('1/13/12', '2014/2/16'))

parse_integer(c('1', '2'))
parse_integer(c('1', '2.0'))
parse_date(c('2012-01-13', '2014-02-16'))
parse_date(c('1/13/12', '2/16/14'))
parse_date(c('2012/1/13', '2014/2/16'))

parse_character(c('2012/1/13', '2014/2/16')) %>% 
  class()

parse_character(c('2012/1/13', '2014/2/16')) %>% 
  lubridate::ymd() %>% 
  class()

tbl_policy_in <- read_csv(
  file.path("data", "policy.csv")
  , col_types = cols(
      region = col_factor()
  )
)

class(tbl_policy_in$region)

library(readxl)

excel_file <- file.path('data', 'read_excel.xlsx')

tbl_musicians <- read_excel(excel_file)

tbl_musicians <- read_excel(
  excel_file
  , sheet = 'leading_rows'
  , skip = 2)

col_names <- read_excel(
  excel_file
  , sheet = 'double_row_name'
  , n_max = 2
  , col_names = FALSE
  , na = " "
  )

col_names <- apply(col_names, 2, paste, collapse = " ") %>% 
  unname() %>%
  gsub("NA", "", x = .) %>% 
  gsub(" ", "_", x = .) %>% 
  gsub("^_", "", x = .) %>% 
  tolower()

tbl_musicians_3 <- read_excel(
  excel_file
  , sheet = 'double_row_name'
  , skip = 2
  , col_names = col_names
  )

save(
  tbl_policy
  , regions
  , file = file.path('data', 'saved_stuff.rda')
)

rm(list = ls())

load(file.path('data', 'saved_stuff.rda'))

ls()

saveRDS(tbl_policy, file = file.path('data', 'tbl_policy.rds'))
saveRDS(regions, file = file.path('data', 'regions.rds'))

rm(list = ls())

tbl_claims <- readRDS(file.path('data', 'tbl_policy.rds'))
useless_garbage <- readRDS(file.path('data', 'regions.rds'))

ls()
