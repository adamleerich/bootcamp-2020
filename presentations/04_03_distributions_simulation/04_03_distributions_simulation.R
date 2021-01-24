source('common.R')

knitr::opts_knit$set(
  warning = FALSE
)

library(tidyverse)

dnorm(0.5)
pnorm(0.5)
qnorm(.975)
rnorm(1e3) %>% 
  mean()

tribble(
    ~Distribution, ~Abbreviation, ~Parameters
  , "Normal", "norm", "mean = 0, sd = 1"
  , "Logormal", "lnorm", "meanlog = 0, sdlog = 1"
  , "Gamma", "gamma", "shape, rate = 1"
  , "Exponential", "exp", "rate = 1"
  , "Poisson", "pois", "lambda"
  , "Negative binomial", "nbinom", "size, prob"
) %>% 
  knitr::kable()

## tibble(
##     prob = seq(.005, .995, by = .005)
##   , x = qnorm(prob)
##   , y = dnorm(x)
## ) %>%
##   ggplot(aes(x, y)) +
##   geom_line()

tibble(
    prob = seq(.005, .995, by = .005)
  , x = qnorm(prob)
  , y = dnorm(x)
) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

library(raw)

data(COTOR2)
emp_cotor <- ecdf(COTOR2)

## tibble(
##     x = seq(0, max(COTOR2), length.out = 500)
##   , y = emp_cotor(x)
## ) %>%
##   ggplot(aes(x, y)) +
##   geom_line()

tibble(
    x = seq(0, max(COTOR2), length.out = 500)
  , y = emp_cotor(x)
) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

## tibble(
##     x = seq(0, 250e3, length.out = 500)
##   , y = emp_cotor(x)
## ) %>%
##   ggplot(aes(x, y)) +
##   geom_line() +
##   scale_x_continuous(labels = scales::comma)

tibble(
    x = seq(0, 250e3, length.out = 500)
  , y = emp_cotor(x)
) %>% 
  ggplot(aes(x, y)) + 
  geom_line() + 
  scale_x_continuous(labels = scales::comma)

library(actuar)

meanlog_est <- 7.9
sdlog_est <- 1.5
mlnorm(1, meanlog = 7.9, sdlog = 1.5)

attachment <- 50e3
limit <- 100e3

lev_upper <- levlnorm(attachment + limit, meanlog = meanlog_est, sdlog = sdlog_est)
lev_lower <- levlnorm(attachment, meanlog = meanlog_est, sdlog = sdlog_est)
ilf <- lev_upper / lev_lower
ilf

rpois(5, 10)
rgamma(5, 2, rate = 5)

sims_policy <- 100
regions <- c('north', 'south', 'east', 'west')
sample_regions <- sample(regions, size = sims_policy, replace = TRUE)

sample_regions %>% 
  head(4)

rnorm(5)
rnorm(5)

set.seed(1234)
rnorm(5)

set.seed(1234)
rnorm(5)

num_policies <- 100
num_claims <- rpois(num_policies, 2)
severity <- rgamma(sum(num_claims), rlnorm(sum(num_claims), 10, .2))
policy_id <- rep(seq_len(num_policies), num_claims)

severity %>% 
  hist()

num_claims %>% 
  hist()

set.seed(8910)
sims <- 5e3

scale <- 250
shape <- 8
severity <- rgamma(sims, shape, scale = scale)

summary(severity)

tibble(x = severity) %>% 
  ggplot(aes(x)) + 
  geom_histogram()

dgamma(severity, 0.5, scale = 500, log = TRUE) %>% 
  sum()

dgamma(severity, 0.5, scale = 1e3, log = TRUE) %>% 
  sum()

log_like_gamma <- function(sample_in, shape_in, scale_in) {
  dgamma(sample_in, shape_in, scale = scale_in, log = TRUE) %>% 
    sum()
}

log_like_gamma(severity, 0.5, 500)
log_like_gamma(severity, 0.5, 1e3)

log_like_gamma(severity, 0.5, c(500, 1e3))

map2_dbl(0.5, c(500, 1e3), log_like_gamma, sample_in = severity)

tbl_log_like <- tibble(
  shape = seq(0.05, 10, length.out = 500)
  , scale = mean(severity) / shape
  , log_like = map2_dbl(shape, scale, log_like_gamma, sample_in = severity)
)

tbl_log_like %>% 
  ggplot(aes(shape, log_like)) + 
  geom_line()

tbl_log_like <- tibble(
  scale = seq(0.01 * min(severity), max(severity) / 2, length.out = 500)
  , shape = mean(severity) / scale
  , log_like = map2_dbl(shape, scale, log_like_gamma, sample_in = severity)
)

tbl_log_like %>% 
  ggplot(aes(scale, log_like)) + 
  geom_line()

tbl_log_like <- crossing(
    scale = seq(0.5 * min(severity), max(severity), length.out = 100)
  , shape = seq(0.05, 10, length.out = 100)
) %>% 
  mutate(
    log_like = map2_dbl(shape, scale, log_like_gamma, sample_in = severity)
    , exp_sev = scale * shape
  )

tbl_sample_mean <- tbl_log_like %>% 
  filter(
    abs(exp_sev - mean(severity)) < 100
  )

## tbl_log_like %>%
##   ggplot(aes(scale, shape)) +
##   geom_raster(aes(fill = log_like), interpolate = TRUE) +
##   scale_fill_continuous(low = 'red', high = 'green') +
##   geom_line(data = tbl_sample_mean, color = 'black') +
##   theme_minimal()

tbl_log_like %>% 
  ggplot(aes(scale, shape)) + 
  geom_raster(aes(fill = log_like), interpolate = TRUE) +
  scale_fill_continuous(low = 'red', high = 'green') + 
  geom_line(data = tbl_sample_mean, color = 'black') + 
  theme_minimal()

tbl_log_like %>% 
  ggplot(aes(scale, shape)) + 
  geom_raster(aes(fill = log_like), interpolate = TRUE) +
  scale_fill_continuous(low = 'red', high = 'green') + 
  geom_line(data = tbl_sample_mean, color = 'black') + 
  theme_minimal()

library(MASS)

fitGamma <- fitdistr(severity, "gamma")
fitLognormal <- fitdistr(severity, "lognormal")
fitWeibull <- fitdistr(severity, "Weibull")

fitGamma
fitLognormal
fitWeibull

## probabilities = seq_len(sims)/(sims + 1)
## 
## weibullQ <- qweibull(probabilities, coef(fitWeibull)[1], coef(fitWeibull)[2])
## lnQ <- qlnorm(probabilities, coef(fitLognormal)[1], coef(fitLognormal)[2])
## gammaQ <- qgamma(probabilities, coef(fitGamma)[1], coef(fitGamma)[2])
## 
## sampleLogMean <- fitLognormal$estimate[1]
## sampleLogSd <- fitLognormal$estimate[2]
## 
## sampleShape <- fitGamma$estimate[1]
## sampleRate <- fitGamma$estimate[2]
## 
## sampleShapeW <- fitWeibull$estimate[1]
## sampleScaleW <- fitWeibull$estimate[2]
## 
## sortedSeverity <- sort(severity)
## oldPar <- par(mfrow = c(1,3))
## plot(sort(weibullQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Weibull Fit")
## abline(0,1)
## 
## plot(sort(lnQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Lognormal Fit")
## abline(0,1)
## 
## plot(sort(gammaQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Gamma Fit")
## abline(0,1)
## 
## par(oldPar)

probabilities = seq_len(sims)/(sims + 1)

weibullQ <- qweibull(probabilities, coef(fitWeibull)[1], coef(fitWeibull)[2])
lnQ <- qlnorm(probabilities, coef(fitLognormal)[1], coef(fitLognormal)[2])
gammaQ <- qgamma(probabilities, coef(fitGamma)[1], coef(fitGamma)[2])

sampleLogMean <- fitLognormal$estimate[1]
sampleLogSd <- fitLognormal$estimate[2]

sampleShape <- fitGamma$estimate[1]
sampleRate <- fitGamma$estimate[2]

sampleShapeW <- fitWeibull$estimate[1]
sampleScaleW <- fitWeibull$estimate[2]

sortedSeverity <- sort(severity)
oldPar <- par(mfrow = c(1,3))
plot(sort(weibullQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Weibull Fit")
abline(0,1)

plot(sort(lnQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Lognormal Fit")
abline(0,1)

plot(sort(gammaQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Gamma Fit")
abline(0,1)

par(oldPar)

## x <- seq(0, max(severity), length.out=500)
## yLN <- dlnorm(x, sampleLogMean, sampleLogSd)
## yGamma <- dgamma(x, sampleShape, sampleRate)
## yWeibull <- dweibull(x, sampleShapeW, sampleScaleW)
## 
## hist(severity, freq=FALSE, ylim=range(yLN, yGamma))
## 
## lines(x, yLN, col="blue")
## lines(x, yGamma, col="red")
## lines(x, yWeibull, col="green")

x <- seq(0, max(severity), length.out=500)
yLN <- dlnorm(x, sampleLogMean, sampleLogSd)
yGamma <- dgamma(x, sampleShape, sampleRate)
yWeibull <- dweibull(x, sampleShapeW, sampleScaleW)

hist(severity, freq=FALSE, ylim=range(yLN, yGamma))

lines(x, yLN, col = "blue")
lines(x, yGamma, col = "red")
lines(x, yWeibull, col = "green")

## sampleCumul <- seq(1, length(severity)) / length(severity)
## stepSample  <- stepfun(sortedSeverity, c(0, sampleCumul), f = 0)
## yGamma <- pgamma(sortedSeverity, sampleShape, sampleRate)
## yWeibull <- pweibull(sortedSeverity, sampleShapeW, sampleScaleW)
## yLN <- plnorm(sortedSeverity, sampleLogMean, sampleLogSd)
## 
## plot(stepSample, col = "black", main = "K-S Gamma")
## lines(sortedSeverity, yGamma, col = "blue")
## 
## plot(stepSample, col = "black", main = "K-S Weibull")
## lines(sortedSeverity, yWeibull, col = "blue")
## 
## plot(stepSample, col = "black", main = "K-S Lognormal")
## lines(sortedSeverity, yLN, col = "blue")

sampleCumul <- seq(1, length(severity)) / length(severity)
stepSample  <- stepfun(sortedSeverity, c(0, sampleCumul), f = 0)
yGamma <- pgamma(sortedSeverity, sampleShape, sampleRate)
yWeibull <- pweibull(sortedSeverity, sampleShapeW, sampleScaleW)
yLN <- plnorm(sortedSeverity, sampleLogMean, sampleLogSd)

plot(stepSample, col = "black", main = "K-S Gamma")
lines(sortedSeverity, yGamma, col = "blue")

plot(stepSample, col = "black", main = "K-S Weibull")
lines(sortedSeverity, yWeibull, col = "blue")

plot(stepSample, col = "black", main = "K-S Lognormal")
lines(sortedSeverity, yLN, col = "blue")

testGamma <- ks.test(severity, "pgamma", sampleShape, sampleRate)
testLN <- ks.test(severity, "plnorm", sampleLogMean, sampleLogSd)
testWeibull <- ks.test(severity, "pweibull", sampleShapeW, sampleScaleW)

testGamma
testLN
testWeibull

quadraticFun <- function(a, b, c){
  function(x) a*x^2 + b*x + c
}

myQuad <- quadraticFun(a=4, b=-3, c=3)

plot(myQuad, -10, 10)

myResult <- optim(8, myQuad)
myResult

myOtherQuad <- quadraticFun(-6, 20, -5)
plot(myOtherQuad, -10, 10)

myResult <- optim(8, myOtherQuad)
myResult
myResult <- optim(8, myOtherQuad, control = list(fnscale=-1))
myResult
