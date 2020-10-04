source('common.R')
knitr::opts_chunk$set(
  warning = FALSE
)

library(raw)
data(RegionExperience)
library(tidyverse)

basePlot <- ggplot(RegionExperience)
class(basePlot)

basePlot

basePlot <- basePlot + aes(x = PolicyYear, y = NumClaims, color=Region)

basePlot

p <- basePlot + geom_line()
p

p <- basePlot + geom_point()
p

p <- basePlot + geom_point() + geom_line()
p

p <- RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumClaims, color = Region)) + 
  geom_line()

p

RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumClaims)) + 
  geom_line(color = Region)

p <- basePlot + geom_bar(stat="identity", aes(fill = Region))
p

p <- basePlot + geom_bar(stat="identity", position="dodge", aes(fill=Region))
p

data(StateExperience)
p <- StateExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumClaims, color = State)) + 
  geom_point() + 
  facet_wrap(~ Region) + 
  theme(legend.position = "none")

p

p <- RegionExperience %>% 
  ggplot(aes(x = PolicyYear, y = NumClaims, group=Region, color=Region)) + 
  geom_point()
p + geom_smooth(se = FALSE)

p + geom_smooth()

p + geom_smooth(method = lm)

p + scale_y_continuous(labels = scales::comma)

p + theme_bw()

data("ppauto")

tbl_yasuda <- ppauto %>% 
  filter(str_detect(Company, "Yasuda")) %>% 
  group_by(AccidentYear) %>% 
  arrange(Lag, .by_group = TRUE) %>% 
  mutate(
    prior_cumulative_paid = lag(CumulativePaid)
    , incremental_paid = coalesce(
      CumulativePaid - prior_cumulative_paid
      , CumulativePaid
    )
  ) %>% 
  ungroup()

tbl_yasuda %>% 
  ggplot(aes(prior_cumulative_paid, incremental_paid)) + 
  geom_point() + 
  facet_wrap(~ Lag, scales = 'free_y') + 
  geom_smooth(method = 'lm', formula = y ~ 0 + x)

tbl_yasuda %>% 
  ggplot(aes(NetEP, incremental_paid)) + 
  geom_point() + 
  facet_wrap(~ Lag, scales = 'free_y') + 
  geom_smooth(method = 'lm', formula = y ~ 0 + x)

tbl_yasuda %>% 
  ggplot(aes(NetEP, incremental_paid)) + 
  geom_point() + 
  facet_wrap(~ Lag, scales = 'free_y') + 
  geom_smooth(method = 'lm', formula = y ~ 1 + x)

