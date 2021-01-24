source('common.R')

op <- par(no.readonly = TRUE)
library(ggplot2)

# Equivalent to 
# plot(x = cars$speed, y = cars$dist)
plot(cars)

ggplot(data = cars) +
  aes(x = speed, y = dist) + 
  geom_point()

x <- rnorm(1e3, mean = 10e3, sd = 20e3)
hist(x)

ggplot() + 
  aes(x = x) + geom_histogram()

x <- rnorm(1e3, mean = 10e3, sd = 20e3)
hist(x, breaks = 20)

ggplot() + aes(x = x) + 
  geom_histogram(bins = 20)

regions <- c(west = 40, south = 20, 
  north = 43, east = 12, midwest = 4)
barplot(regions)

ggplot() + 
  aes(x = names(regions), y = regions) + 
  geom_col()

boxplot(
  Sepal.Length ~ Species, 
  data = iris)

ggplot(data = iris) + 
  aes(x = Species, y = Sepal.Length) + 
  geom_boxplot()

x <- rnorm(1000)
qqnorm(x)

ggplot() + aes(sample = x) + 
  stat_qq()

model <- lm(Sepal.Width ~ Petal.Width + Species, data = iris)
plot(model)


plot(iris)

GGally::ggpairs(iris)

plot(cars)
abline(
  lm(cars$dist ~ cars$speed), 
  lwd = 2, 
  col = 'blue')

ggplot(data = cars) + 
  aes(x = speed, y = dist) + 
  geom_point() + geom_smooth(
    formula = y ~ x, 
    method = "lm", se = FALSE)

plot(
  cars, main = 'Cars',  
  xlab = 'mph',
  ylab = 'ft')

ggplot(data = cars) + 
  aes(x = speed, y = dist) + 
  geom_point() + ggtitle('Cars') +
  xlab('mph') + ylab('Distance ft')
