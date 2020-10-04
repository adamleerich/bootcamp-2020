#' ---
#' title:   "Capstone Day: Prepare Data"
#' author:  "Adam Rich"
#' date:    "September 24, 2020"
#' ---



#' The goal is to create two datasets
#' 
#'   * `pol_final` for analyzing frequency
#'   * `claims_final` for analyzing severity
#' 
#'     
#' There are a few steps that have to be done
#' 
#'   1. Load the four data files to memory
#'   1. Spread `pol_rating` 
#'   1. Join the new wide `pol_rating` object to `pol_dates`
#'   1. Join with the state lookup table 
#'   1. Put rating characteristics back in claims table
#'   1. Aggregate claims data by policy
#'   1. Join agg claims with policy data
#'   1. Add some derived columns
#'   1. Do some sense checking
#'   1. Save files
#'   

#' I'll use `tidyverse` package because it automatically loads `dplyr` and `tidyr`.
#' I'll need `tidyr` to "reshape" or "spread" the `pol_rating` data object.
#' The capstone project ZIP also came with *resources.R* so let's `source` that, too.
DirectoryPath <- getwd()
library(tidyverse)
source(file.path(DirectoryPath ,'resources.R'))

#' ## Load the four data files to memory

claims <- read_csv(file.path(DirectoryPath ,'claims.csv') ,guess_max = 15000)

pol_dates <- read_csv(file.path(DirectoryPath ,'pol_dates.csv') ,guess_max = 15000)

pol_rating <- read_csv(file.path(DirectoryPath ,'pol_rating.csv') ,guess_max = 150000)

#' `read_csv` is reading `value` column as double.
#' But, that means we are losing all character data.
#' Force the function to load `value` as character,
#' and we can convert numbers after reshaping with
#' `tidyr::spread`.
# pol_rating <- read_csv(
#   "ratemaking-capstone/pol_rating.csv",
#   col_types = 'dccc')

state_lookup <- read_csv(file.path(DirectoryPath ,'states.csv'))

#' This is what the four data frames look like.

head(claims)
# str(claims)
glimpse(claims)

head(pol_dates)
# str(pol_dates)
glimpse(pol_dates)

head(pol_rating)
# str(pol_rating)
glimpse(pol_rating)

head(state_lookup)
# str(state_lookup)
glimpse(state_lookup)

## Checking uniqueness of policy_numbers
pol_dates %>%
  summarise(TotalCount = n()
            ,Distinct  = n_distinct(policy_number))

#' ## Spread `pol_rating` 

# Remove X1 before spreading
# If you don't, the data will not collapse 
# to one row per policy
pol_rating_wide <- pol_rating %>% 
  select(-X1) %>% 
  spread(key = variable, value = value)

head(pol_rating_wide)
# str(pol_rating_wide)
glimpse(pol_rating_wide)

# Convert numeric columns
is_all_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

pol_rating_wide <- pol_rating_wide %>%
  mutate_if(is_all_numeric ,as.numeric)


head(pol_rating_wide)
# str(pol_rating_wide)
glimpse(pol_rating_wide)

#' ## Join the new wide `pol_rating` object to `pol_dates`

pol <- pol_dates %>% 
  select(-X1) %>% 
  dplyr::inner_join(pol_rating_wide ,by = "policy_number")
  

head(pol)
# str(pol)
glimpse(pol)


#' ## Join with the state lookup table 

#' The data frames `state_lookup` and `pol` do **not** spell "state" the same.
#' One has an uppercase 'S' the other lowercase.
#' Since R is case-sensitive, this is a problem.
#' This statement would give an error


#+ eval=FALSE
# Gives an error!
#
#   pol_state <- pol %>% inner_join(state_lookup)
#


#' One option is to use the `by` arg of `inner_join`.
pol_state <- pol %>% 
  inner_join(state_lookup, by = c("state" = "State"))

#' Another option is to rename the columns of `state_lookup` first.
#' Then do the join.
#' I like this one because I want to rename the other columns anyway.

# names(state_lookup) <- c('state', 'state_group', 'state_population')
# pol_state <- pol %>% 
#   inner_join(state_lookup)

#' OK, let's do this a third time, because I actually 
#' don't want `state_population` in the joined table.


state_lookup <- state_lookup %>%
  rename(state_group = `Frequency Group`)

state_group_lookup <- state_lookup[, c('State', 'state_group')]
pol_state <- pol %>% 
  inner_join(state_group_lookup ,by = c("state" = "State"))

head(pol_state)
# str(pol_state)
glimpse(pol_state)


#' ## Put rating characteristics back in claims table
#' 
#' Let's do this step before aggregating the claims data
#' and adding to the policy data.
#' The reason is that I don't need "total claim count" by policy 
#' added back to the claims data.

claims_final <- claims %>% 
  select(-X1) %>% 
  inner_join(pol_state ,by = "policy_number")

head(claims_final)
# str(claims_final)
glimpse(claims_final)





#' ## Aggregate claims data by policy
#' 
#' Like everything in R there is more than one way to do this.
#' Who knows which is better...

# One option for aggregating claims
claims_agg <- claims %>% 
  group_by(policy_number) %>% 
  summarize(
    total_ultimate = sum(claim_ultimate), 
    claim_count = n())

# Another option for aggregating claims
# claims$count <- 1
# claims_agg <- claims %>% 
#   group_by(policy_number) %>% 
#   summarize(
#     total_ultimate = sum(claim_ultimate), 
#     claim_count = sum(count))


head(claims_agg)
# str(claims_agg)
glimpse(claims_agg)


#' ## Join agg claims with policy data
#' 
#' Doing the join is easy.
#' But because this is a left join,
#' Any policies without claims will have NAs in the `total_ultimate`
#' and `claim_count` columns.
#' But, this will cause problems in modeling later,
#' so we will change NAs in these columns to 0.

pol_final <- pol_state %>%
  left_join(claims_agg ,by = "policy_number")

#' There are different ways to replace NAs with zeros.
#' I'll use one for `total_ultimate`.
pol_final <- pol_final %>%
  mutate(total_ultimate = replace_na(total_ultimate ,0)
         ,claim_count   = replace_na(claim_count ,0))


head(pol_final)
# str(pol_final)
glimpse(pol_final)



#' ## Add some derived columns

# I want this column in both!
pol_final <- pol_final %>%
  mutate(years_in_business = year_yyyymm(inception) - as.integer(year_started) + 1)

claims_final <- claims_final %>% 
  mutate(years_in_business = year_yyyymm(inception) - as.integer(year_started) + 1)

pol_final <- pol_final %>%
  mutate(average_severity = if_else(pol_final$claim_count == 0, 0, pol_final$total_ultimate / pol_final$claim_count))

head(pol_final)
head(claims_final)
glimpse(pol_final)
glimpse(claims_final)





#' ## Do some sense checking
#' 
#' You should always check that you didn't lose or make up any data
#' especially when doing joins.

# Next two statements need to output the same number.
sum(pol_final$claim_count)
nrow(claims)

# Next three statements need to output the same number.
sum(pol_final$total_ultimate)
sum(claims$claim_ultimate)
sum(pol_final$average_severity * pol_final$claim_count)

# Next three statements need to output the same number.
nrow(pol_final)
length(unique(pol_final$policy_number))
length(unique(pol$policy_number))


#' ## Save files
#' 
#' The first commented out statements
#' get fancy and create a timestamped
#' output file
#' 

# fname <- paste0(
#   'ratemaking-capstone/data-', 
#   format(Sys.time(), '%Y-%m-%d-%H%M'), 
#   '.RData')
# print(fname)
# save(pol_final, claims_final, file = fname)
# save(pol_final, claims_final, file = file.path(DirectoryPath ,'reshaped_data.Rdata'))

write_csv(pol_final ,path = file.path(DirectoryPath ,'pol_final.csv'))
write_csv(claims_final ,path = file.path(DirectoryPath ,'claims_final.csv'))


