source('common.R')

## install.packages('tree')
## library(tree)
## library(tidyverse)
## library(MASS)         # To get example data
## library(raw)          # To get example data
library(tree)
library(tidyverse)
library(MASS)
library(raw)

data(Boston)
head(Boston)

bos <- Boston %>% 
  mutate(medv_pr = medv / rm) %>% 
  dplyr::select(age, dis, medv_pr) %>% 
  mutate(medv_prf = cut(medv_pr, breaks = 0:5 * 2))

p <- ggplot(data = bos) +
  aes(x = age, y = dis, color = medv_pr) +
  geom_point(mapping = ) +
  scale_color_gradientn(
    colors = c('yellow', 'green', 'blue', 'red'))

print(p)

tbos <- tree(
  formula = medv_pr ~ age + log(dis),
  data = bos
)

plot(tbos, type = 'uniform')
text(tbos, pretty = 5, col = 'blue', cex = 0.8)

rm(list = ls())

## load('your_path/reshaped_data.Rdata')
## ls()
load('c:/home/git/other/r_bootcamp/ratemaking-capstone/reshaped_data.Rdata')
ls()

str(pol_final)

pol_4tree <- pol_final %>% 
  mutate(has_claims = (claim_count > 0)) %>% 
  dplyr::select(
    has_claims, discipline, 
    employee_count, five_year_claims, 
    revenue, state_group, years_in_business,
    claim_count) %>% 
  mutate(
    has_claims = as.integer(has_claims),
    discipline = as.factor(discipline),
    state_group = as.factor(state_group))

tpol <- tree(
  formula = claim_count ~ revenue + discipline + state_group,
  data = pol_4tree)

plot(tpol, type = 'uniform')
text(tpol, pretty = 5, col = 'blue', cex = 0.8)

tpol <- tree(
  formula = claim_count ~ discipline + state_group,
  data = pol_4tree,
  mindev = 0.001,
  minsize = 500)
## print(tpol)
tree:::print.tree(tpol)

plot(tpol, type = 'uniform')
text(tpol, pretty = 5, col = 'blue', cex = 0.8)

pol_4tree$big <- (pol_4tree$revenue >= 4e6)
table(pol_4tree$big)

p1 <- pol_4tree %>% 
  mutate(rev_band = round(revenue/10e3, 0)) %>% 
  group_by(rev_band) %>% 
  summarize(avg_claim_count = mean(claim_count)) %>% 
  ggplot() + 
  aes(rev_band, avg_claim_count) +
  geom_point()

p1

pol_4tree$lrevenue <- log(pol_4tree$revenue)
summary(pol_4tree$lrevenue)

ggplot(data = pol_4tree) +
  aes(lrevenue) +
  geom_histogram()

p2 <- pol_4tree %>% 
  mutate(lrev_band = round(lrevenue, 2)) %>% 
  group_by(lrev_band) %>% 
  summarize(avg_claim_count = mean(claim_count)) %>% 
  ggplot() + 
  aes(lrev_band, avg_claim_count) +
  geom_point()

p2
