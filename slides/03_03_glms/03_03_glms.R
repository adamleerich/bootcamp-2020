source('common.R')

knitr::opts_chunk$set(
  warning = FALSE
)

library(tidyverse)

num_sims <- 5e3
b_0 <- 5
b_1 <- -3

set.seed(1234)
tbl_ols <- tibble(
  x_1 = runif(num_sims, 0, 10)
  , e = rnorm(num_sims, mean = 0, sd = 1)
  , y = b_0 + b_1 * x_1 + e
)

set.seed(1234)
tbl_ols <- tibble(
  x_1 = runif(num_sims, 0, 10)
  , mu = b_0 + b_1 * x_1
  , y = rnorm(num_sims, mean = mu, sd = 1)
)

tbl_ols <- tibble(
  x_1 = runif(num_sims, 0, 10)
  , e = rnorm(num_sims, mean = 0, sd = 1)
  , y = b_0 + b_1 * x_1 + e
)

tbl_ols <- tibble(
  x_1 = runif(num_sims, 0, 10)
  , mu = b_0 + b_1 * x_1
  , y = rnorm(num_sims, mean = mu, sd = 10)
)


set.seed(1234)

tbl_poisson <- tibble(
    x = sample(c(2, 4, 6), num_sims, replace = TRUE, prob = c(2, 1, 2))
  , mu = x
  , y = rpois(num_sims, mu)
)

tbl_poisson %>% 
  ggplot(aes(x, y)) + 
  geom_boxplot(aes(group = x))

est_mus <- tbl_poisson %>% 
  group_by(x) %>% 
  summarise(est_mu = mean(y))

tbl_poisson <- tbl_poisson %>% 
  inner_join(est_mus, by = 'x') %>% 
  rename(predicted_three_pois = est_mu) %>% 
  mutate(
      residual_three_pois = y - predicted_three_pois
    , residual_three_pois = residual_three_pois / sd(residual_three_pois)
  )

tbl_poisson %>% 
  ggplot(aes(x, residual_three_pois)) + 
  geom_hline(color = 'red', yintercept = 0) + 
  geom_boxplot(aes(group = x))

fit_ols <- lm(
    formula = y ~ 0 + x
  , data = tbl_poisson
)

tbl_poisson <- tbl_poisson %>% 
  mutate(
      residuals_ols = rstandard(fit_ols)
    , predicted_ols = predict(fit_ols)
  )

tbl_poisson %>% 
  ggplot(aes(predicted_ols, residuals_ols)) + 
  geom_hline(color = 'red', yintercept = 0) + 
  geom_jitter(height = 0)

transform_x <- function(x, beta, intercept = 0){

  p <- length(beta)
  x <- matrix(x, ncol = p)
  beta <- matrix(beta, nrow = p)
  
  transformed_x <- x %*% beta %>% 
    as.vector()
  
  intercept + transformed_x  
}

dpois(tbl_poisson$y, transform_x(tbl_poisson$x, 0.9), log = TRUE) %>% 
  sum()
dpois(tbl_poisson$y, transform_x(tbl_poisson$x, 1.0), log = TRUE) %>% 
  sum()
dpois(tbl_poisson$y, transform_x(tbl_poisson$x, 1.1), log = TRUE) %>% 
  sum()

fit_glm <- glm(
    formula = y ~ 0 + x
  , data = tbl_poisson
  , family = poisson(link = 'identity')
)

logLik(fit_glm)

fit_glm$coefficients

dpois(
    tbl_poisson$y
  , transform_x(tbl_poisson$x, fit_glm$coefficients, 0)
  , log = TRUE) %>% 
  sum()

b0 <- 1
b1 <- 3
b2 <- -1/6
set.seed(1234)
tbl_poisson <- tibble(
    x_1 = runif(num_sims, 1, 10)
  , x_2 = runif(num_sims, 10, 50)
  , eta = matrix(c(x_1, x_2), ncol = 2) %>% 
      transform_x(c(b1, b2), b0)
  , mu = eta
  , y = rpois(num_sims, mu)
)

tbl_poisson %>% 
  ggplot(aes(mu, y)) + 
  geom_point()

tbl_poisson %>% 
  ggplot(aes(mu)) + 
  geom_histogram()

b0 <- 1
b1 <- log(40) / 9
b2 <- log(0.25) / 30

set.seed(1234)
tbl_poisson <- tibble(
    x_1 = runif(num_sims, 1, 10)
  , x_2 = runif(num_sims, 10, 50)
  , eta = matrix(c(x_1, x_2), ncol = 2) %>% 
      transform_x(c(b1, b2), b0)
  , mu = exp(eta)
  , y = rpois(num_sims, mu)
)

tbl_poisson$eta %>% summary()
tbl_poisson$y %>% summary()

tribble(
    ~Distribution, ~`Canonical Link`, ~`$g(x)$`
  , 'binomial', 'logit', '$g(x)=\\frac{exp(x)}{1+exp(x)}$'
  , 'gaussian', 'identity', '$g(x)=x$'
  , 'poisson', 'log' , '$g(x)=ln(x)$'
  , 'Gamma', 'inverse', '$g(x)=1/x$'
) %>% 
  knitr::kable(
    escape = FALSE
  )

predict(fit_pois) %>% head()
predict(fit_pois, type = 'response') %>% head()

predict(fit_pois) %>% head() %>% fit_pois$family$linkinv()

library(broom)

tbl_model_diagnostics <- fit_pois %>% 
  glance()

tbl_model_diagnostics %>% 
  knitr::kable(caption = "Model diagnostics for a Poisson GLM")

toy_rows <- 10
tbl_poisson_toy <- tbl_poisson %>% 
  slice(seq_len(toy_rows))

fit_worst <- tbl_poisson_toy %>% 
  glm(
      formula = y ~ 1
    , family = poisson
  )

logLik(fit_worst)

my_matrix <- diag(toy_rows)
colnames(my_matrix) <- paste0("P", seq_len(toy_rows))
my_matrix <- my_matrix[, -toy_rows]

my_matrix %>% 
  knitr::kable()

tbl_poisson_toy <- tbl_poisson_toy %>% 
  bind_cols(as_tibble(my_matrix))

best_formula <- paste(
    "y ~ 1 + "
  , paste(colnames(my_matrix), collapse = "+")) %>% 
  as.formula()

best_formula

fit_best <- tbl_poisson_toy %>% 
  glm(
      formula = best_formula
    , family = poisson
  )

logLik(fit_best)
logLik(fit_worst)

as.double(2 * (logLik(fit_best) - logLik(fit_worst)))
fit_best$null.deviance
fit_worst$null.deviance

fit_toy <- tbl_poisson_toy %>% 
  glm(
    formula = y ~ 1 + x_1 + x_2
  , family = poisson
)

fit_toy$deviance
as.double(2 * (logLik(fit_best) - logLik(fit_toy)))

fit_pois <- glm(
    formula = y ~ 1 + x_1 + x_2
  , data = tbl_poisson
  , family = poisson
)

fit_pois$null.deviance
fit_pois$deviance

fit_pois_x1 <- glm(
    data = tbl_poisson
  , formula = y ~ 1 + x_1
  , family = poisson
)

fit_pois_x1$deviance
fit_pois$deviance

fit_pois_simple <- tbl_poisson %>% 
  glm(
      formula = y ~ 1 + x_1
    , family = poisson
  )

fit_pois_simple$aic
fit_pois$aic

tbl_coef_diagnostics <- fit_pois %>% 
  broom::tidy()

tbl_coef_diagnostics %>% 
  knitr::kable(
    caption = 'Coefficient diagnostics'
  )

tribble(
  ~Family, ~`$d_i$`
  , "Normal", "$\\left(y_i - \\hat{\\mu}\\right)^2$"
  , "Poissson", "$2*\\left(y * log\\left(y_i / \\hat{\\mu}\\right) - \\left(y_i - \\hat{\\mu} \\right)  \\right)$"
  , "binomial", "$2*\\left(y_i * log(y_i / \\hat{\\mu}) + (m - y_i) log\\left[\\frac{m - y_i}{m - \\hat{\\mu}} \\right] \\right)$"
  , "gamma", "$2*\\left(-log\\left(y_i / \\hat{\\mu} \\right) + \\frac{\\left(y_i - \\hat{\\mu} \\right)}{\\hat{\\mu}} \\right)$"
) %>% 
  knitr::kable(
    caption = 'Individual deviance expressions'
    , escape = FALSE
  )

hat_matrix <- fit_pois %>% 
  influence() %>% 
  pluck('hat')

phi <- fit_pois %>% 
  summary() %>% 
  pluck('dispersion')

tbl_residual <- tbl_poisson %>% 
  dplyr::select(y) %>% 
  mutate(
      predicted_response = predict(fit_pois, type = 'response')
    , resid_deviance = residuals(fit_pois)
    , resid_pearson = rstandard(fit_pois, type = 'pearson')
  ) %>% 
  mutate(
      hat = hat_matrix
    , phi = phi
    , scale_factor = phi * sqrt((1 - hat))
    , resid_deviance_standard_manual = resid_deviance / scale_factor
    , resid_deviance_standard = rstandard(fit_pois, type = 'deviance')
  )

tbl_residual %>% 
  dplyr::select(resid_deviance, resid_deviance_standard_manual, resid_deviance_standard) %>% 
  head() %>% 
  knitr::kable(
    caption = 'Manual calculation of standardized deviance residuals'
  )

tbl_residual %>% 
  ggplot(aes(predicted_response, resid_deviance)) + 
  geom_point()

tribble(
    ~Distribution, ~Transform
  , 'Normal', '$\\hat{\\mu}$'
  , 'Poisson', '$2*sqrt{\\hat{\\mu}}$'
  , 'Binomial', '$2*sin^-1\\sqrt{\\hat{\\mu}}$'
  , 'Gamma', '$2*log{\\hat{\\mu}}$'
  , 'Inverse Gaussian', '$-2\\hat{\\mu}^{-1/2}$'
  ) %>%
  knitr::kable(
      caption = 'Residual scale transformations'
    , escape = FALSE
  )

tbl_residual <- tbl_residual %>% 
  mutate(
      predicted_response_scaled = 2 * (predicted_response %>% sqrt)
  )

plt_standard <- tbl_residual %>% 
  ggplot(aes(predicted_response, resid_deviance_standard)) + 
  geom_point()

plt_adj <- tbl_residual %>% 
  ggplot(aes(predicted_response_scaled, resid_deviance_standard)) + 
  geom_point()

library(gridExtra)
grid.arrange(plt_standard, plt_adj, nrow = 1)

set.seed(1234)
tbl_offset <- tibble(
    payroll = runif(num_sims, 10e3, 100e3)
  , lambda = .01
  , claim_count = rpois(num_sims, payroll * lambda)
)

fit_no_offset <- glm(
    formula = claim_count ~ 1 + payroll
  , data = tbl_offset
  , family = "poisson")

fit_offset <- glm(
    formula = claim_count ~ 1
  , data = tbl_offset
  , family = "poisson"
  , offset = log(payroll))

fit_no_offset$aic
fit_offset$aic
coef(fit_offset) %>% exp()

logistic <- function(x) {
  exp(x) / (exp(x) + 1)
}

tibble(
    x = runif(num_sims, -7, 7)
  , y = logistic(x)
) %>% 
  ggplot(aes(x, y)) + 
  geom_line()

b0 <- -5
b1 <- 0.1
b2 <- 0.1
num_sims <- 500

set.seed(1234)
tbl_logistic <- tibble(
    x_1 = runif(num_sims, 0, 100)
  , x_2 = runif(num_sims, -10, 60)
  , eta = transform_x(cbind(x_1, x_2), c(b1, b2), b0)
  , mu = logistic(eta)
  , claim = rbinom(num_sims, 1, mu)
)

plt_x_1 <- tbl_logistic %>% 
  ggplot(aes(x_1, claim)) + 
  geom_jitter(height = 0.2)

plt_x_2 <- tbl_logistic %>% 
  ggplot(aes(x_2, claim)) + 
  geom_jitter(height = 0.2)

grid.arrange(
    plt_x_1
  , plt_x_2
  , nrow = 1
)

fit_logistic <- glm(
    formula = claim ~ 1 + x_1 + x_2
  , data = tbl_logistic
  , family = binomial(link = 'logit')
)

tbl_prob_50 <- crossing(
    x_1 = seq(0, 75, length.out = 500)
  , x_2 = seq(-10, 60, length.out = 500)
) %>% 
  mutate(
    p = predict(fit_logistic, newdata = ., type = 'response')
  ) %>% 
  filter(
    p > 0.49
  , p < 0.51
  )

tbl_logistic %>%
  mutate(claim = as.factor(claim)) %>% 
  ggplot(aes(x_1, x_2, color = claim)) +
  geom_point() +
  geom_line(data = tbl_prob_50, aes(x_1, x_2), color = 'black')

fit_logistic_x1 <- glm(
    data = tbl_logistic
  , formula = claim ~ 1 + x_1
  , family = binomial()
)

tbl_prob_50_x1 <- tibble(
    x_1 = seq(0, 75, length.out = 500)
  ) %>% 
  mutate(
    p = predict(fit_logistic_x1, newdata = ., type = 'response')
    , diff = abs(p - 0.5)
  ) %>% 
  dplyr::top_n(-1, diff)

tbl_logistic %>%
  ggplot(aes(x_1, claim, color = as.factor(claim))) +
  geom_jitter() +
  geom_vline(data = tbl_prob_50_x1, aes(xintercept = x_1)) +
  labs(colour = 'claim')

tbl_logistic %>%
  ggplot(aes(x_1, fill = as.factor(claim))) +
  geom_density(alpha = 0.6) +
  geom_vline(data = tbl_prob_50_x1, aes(xintercept = x_1)) + 
  labs(fill = 'claim')

wrangle_triangle <- function(tbl_in) {
  
  tbl_in %>% 
    group_by(GroupCode, AccidentYear) %>% 
    arrange(Lag, .by_group = TRUE) %>% 
    mutate(
      prior_paid = dplyr::lag(CumulativePaid)
      , prior_incurred = dplyr::lag(CumulativeIncurred)
      , incremental_paid = coalesce(CumulativePaid - prior_paid, CumulativePaid)
      , incremental_incurred = coalesce(CumulativeIncurred - prior_incurred, CumulativeIncurred)
      , ldf_paid = CumulativePaid / prior_paid
      , ldf_incurred = CumulativeIncurred / prior_incurred
    ) %>% 
    ungroup() %>% 
    mutate(
      lag_factor = as.factor(Lag)
      , ay_factor = as.factor(AccidentYear)
      , upper = DevelopmentYear <= 1997
    )
}

library(raw)
data(medmal)
data(NJM_WC)

tbl_njm_wc <- NJM_WC %>% 
  wrangle_triangle()

tbl_medmal <- medmal %>% 
  wrangle_triangle()

tbl_upper <- tbl_medmal %>% 
  filter(upper)

tbl_lower <- tbl_medmal %>% 
  filter(!upper)

tbl_upper <- tbl_upper %>% 
  filter(Lag > 1)

tbl_njm_wc <- tbl_njm_wc %>% 
  filter(Lag > 1)

fit_upper <- tbl_upper %>% 
  glm(
    formula = incremental_paid ~ 0 + prior_paid:lag_factor
    , data = .
    , family = poisson
  )

tbl_upper <- tbl_upper %>% 
  group_by(lag_factor) %>% 
  mutate(
    shift = -min(incremental_paid) %>% pmax(0)
    , incremental_paid_shifted = incremental_paid + shift
  ) %>% 
  ungroup()

fit_upper <- tbl_upper %>% 
  glm(
    formula = incremental_paid_shifted ~ 0 + prior_paid:lag_factor
    , data = .
    , family = poisson()
  )

tbl_upper <- tbl_upper %>% 
  mutate(
    predicted = predict(fit_upper, type = 'response') - shift
  )

tbl_upper %>% 
  ggplot(aes(prior_paid)) + 
  geom_point(aes(y = incremental_paid)) + 
  geom_point(aes(y = predicted), color = 'red') + 
  scale_x_log10() +
  facet_wrap(~ lag_factor, scales = 'free')

fit_njm_wc <- tbl_njm_wc %>% 
  glm(
      formula = incremental_paid ~ 0 + prior_paid:lag_factor
    , data = .
    , family = poisson(link = 'identity')
  )

fit_njm_wc$coefficients %>% 
  fit_njm_wc$family$linkinv() %>% 
  unname()

tbl_njm_wc %>% 
  group_by(Lag) %>% 
  summarise(
    weighted_ldf = sum(incremental_paid) / sum(prior_paid)
  ) %>% 
  pull(weighted_ldf)
