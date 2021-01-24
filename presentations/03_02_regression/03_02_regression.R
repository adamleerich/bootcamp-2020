source('common.R')

library(tidyverse)

num_sims <- 1e3
b_0 <- 5
b_1 <- -3
b_2 <- 2.5
regions <- c('north', 'south', 'east', 'west')

set.seed(1234)
tbl_ols <- tibble(
  x_1 = runif(num_sims, 0, 10)
  , x_2 = runif(num_sims, 30, 60)
  , x_3 = runif(num_sims, 200, 300)
  , x_4 = sample(regions, num_sims, replace = TRUE)
  , e = rnorm(num_sims, mean = 0, sd = 1)
)

tbl_ols <- tbl_ols %>% 
  mutate(
    y = b_0 + b_1 * x_1 + b_2 * x_2 + e
    , y = y + case_when(
      x_4 == 'north' ~ 10
      , x_4 == 'south' ~ -20
      , x_4 == 'east' ~ 50
      , x_4 == 'west' ~ -70
    )
  )

tbl_ols %>%
  select(-e) %>% 
  plot()

tbl_ols %>% 
  boxplot(y ~ x_4, data = .)

tbl_ols %>%
  select(x_1, x_2, x_3) %>% 
  cor()

fit_1 <- tbl_ols %>% 
  lm(
    formula = y ~ x_1
  )

# tired
predict(fit_1) %>% 
  head()

# wired
tbl_ols <- tbl_ols %>% 
  mutate(
    predict_one = predict(fit_1)
  )

tbl_ols %>% 
  model.matrix(y ~ x_1, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ 1 + x_1, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ x_1 + x_2, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ x_1 + x_1:x_2, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ x_1*x_2, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ 0 + x_1*x_2*x_3, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ x_4, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ 0 + x_1 + x_4, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ 0 + x_1:x_4, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ 0 + x_1 + x_4, data = .) %>% 
  head()

tbl_ols %>% 
  model.matrix(y ~ 0 + x_1:x_4, data = .) %>% 
  head()

summary(fit_1)

tbl_fit_one <- tbl_ols %>% 
  mutate(
    y_hat = predict(fit_1)
  )

tbl_fit_one %>% 
  ggplot(aes(x_1)) + 
  geom_point(aes(y = y)) + 
  geom_line(aes(y = y_hat))

tbl_fit_one %>% 
  ggplot(aes(y_hat, y)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red")

tbl_fit_one <- tbl_fit_one %>% 
  mutate(
    residual = residuals(fit_1)
    , resid_standard = rstandard(fit_1)
  )

tbl_fit_one %>% 
  ggplot(aes(y_hat, residual)) + 
  geom_point()

tbl_fit_one %>% 
  ggplot(aes(y_hat, resid_standard)) + 
  geom_point() + 
  geom_hline(yintercept = c(-3,3), color = "red")

tbl_fit_one %>% 
  ggplot(aes(x_2, resid_standard)) + 
  geom_point() + 
  geom_hline(yintercept = c(-3,3), color = "red")

fit_2 <- tbl_ols %>% 
  lm(formula = y ~ x_1 + x_2)

fit_3 <- tbl_ols %>% 
  lm(formula = y ~ x_1 + x_2 + x_3)

fit_4 <- tbl_ols %>% 
  lm(formula = y ~ x_1 + x_3)

augment_model <- function(tbl_in, fit_in) {

  tbl_in %>% 
    mutate(
      residual = residuals(fit_in)
      , resid_standard = rstandard(fit_in)
      , y_hat = predict(fit_in)
    )  
}

tbl_fit_two <- tbl_ols %>% augment_model(fit_2)
tbl_fit_three <- tbl_ols %>% augment_model(fit_3)
tbl_fit_four <- tbl_ols %>% augment_model(fit_4)

tbl_fit_one %>% ggplot(aes(y_hat, resid_standard)) + 
  geom_point(color = 'blue') + 
  geom_point(color = 'green', data = tbl_fit_two) + 
  geom_point(color = 'orange', data = tbl_fit_three) + 
  geom_point(color = 'pink', data = tbl_fit_four)  

fit_5 <- tbl_ols %>%
  select(-e) %>% 
  lm(formula = y ~ .)

tbl_fit_five <- tbl_ols %>% augment_model(fit_5)

tbl_fit_five %>% ggplot(aes(y_hat, resid_standard)) + 
  geom_point(color = 'blue')

library(broom)

fit_5 %>% 
  tidy()

lst_fits <- list(
  one = fit_1
  , two = fit_2
  , three = fit_3
  , four = fit_4
  , five = fit_5
)

tbl_fits <- purrr::map_dfr(
  lst_fits
  , glance)

tbl_fits <- tbl_fits %>% 
  mutate(model_name = names(lst_fits)) %>% 
  select(model_name, everything())

tbl_fits %>% 
  select(1:6) %>% 
  knitr::kable()
  
tbl_fits %>% 
  select(1, 7:12) %>% 
  knitr::kable()
