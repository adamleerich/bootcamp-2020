source('common.R')

## install.packages('tree')
## install.packages('randomForest')
## library(tree)
## library(randomForest)
## library(tidyverse)
## library(MASS)         # To get example data
## library(raw)          # To get example data
library(tree)
library(randomForest)
library(tidyverse)
library(MASS)
library(raw)

data(Boston)
head(Boston)

bos <- Boston %>% 
  dplyr::select(dis, rm, medv) %>% 
  mutate(medv_f = cut(medv, breaks = 0:5 * 10))

plot(bos)

plot(medv ~ rm, data = bos)

plot(medv ~ log(dis), data = bos)

p <- ggplot(data = bos) +
  aes(x = rm, y = log(dis), color = medv) +
  geom_point() +
  scale_color_gradientn(
    colors = c('yellow', 'green', 'blue', 'red'))

print(p)

tbos <- tree(
  formula = medv ~ rm + log(dis),
  data = bos
)
summary(tbos)

plot(tbos)

plot(tbos)
text(tbos)

plot(tbos, type = 'uniform')
text(tbos, pretty = 5, col = 'blue', cex = 0.8)

data(RegionExperience)
RegionExperience %>% head

plot(RegionExperience)

table(RegionExperience$Region)
table(RegionExperience$PolicyYear)

rex <- RegionExperience %>% 
  mutate(Freq = NumClaims / NumPolicies) %>% 
  dplyr::select(Region, PolicyYear, Freq)
table(rex$Region)

trex <- tree(
  formula = Freq ~ .,
  data = rex
)

plot(trex, type = 'uniform')
text(trex, pretty = 10, col = 'blue', cex = 0.8)

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

rf <- randomForest(
  medv ~ ., data = Boston, 
  ntree = 100,
  nodesize = 100,
  importance = TRUE,
  mtry = 3)
rf

plot(rf)

importance(rf)

par(mfrow = c(1, 2))

varImpPlot(rf)
