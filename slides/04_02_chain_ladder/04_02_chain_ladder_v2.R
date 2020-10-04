source('common.R')

## install.packages('ChainLadder')
## library(ChainLadder)
## library(tidyverse)
library(ChainLadder)
library(tidyverse)

asl17g <- read.csv('http://www.casact.org/research/reserve_data/othliab_pos.csv')
names(asl17g)

## # What GROUPs are here?
## sort(unique(asl17g$GRNAME))

length(unique(asl17g$GRNAME))

asl17g$GRNAME %>% unique %>% length

asl17g %>% 
  group_by(GRNAME) %>% 
  summarize() %>% 
  nrow

nrow(asl17g)
nrow(unique(asl17g[, c('GRCODE', 'AccidentYear', 'DevelopmentYear')]))
nrow(unique(asl17g[, c('GRCODE', 'AccidentYear', 'DevelopmentLag')]))

nrow(unique(asl17g[, c('GRCODE', 'AccidentYear')]))
nrow(unique(asl17g[, c('GRCODE', 'DevelopmentLag')]))
nrow(unique(asl17g[, c('AccidentYear', 'DevelopmentLag')]))

asl17g %>% 
  dplyr::filter(DevelopmentYear == 1997) %>% 
  group_by(AccidentYear) %>% 
  summarize(TotalPremium = sum(EarnedPremDIR_h1))

asl17 <- asl17g %>% 
  rename(
    AY = AccidentYear,
    DY = DevelopmentYear,
    Dev = DevelopmentLag) %>% 
  group_by(AY, DY, Dev) %>% 
  summarize(
    UltLoss = sum(IncurLoss_h1),
    PdLoss = sum(CumPaidLoss_h1),
    IBNR = sum(BulkLoss_h1),
    GPE = sum(EarnedPremDIR_h1)) %>% 
  ungroup() %>% 
  mutate(
    IncLoss = UltLoss - IBNR)

tri.inc <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'IncLoss')

tri.pd <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'PdLoss')

tri.gpe <- as.triangle(
  asl17[asl17$DY <= 1997, ], 
  origin = 'AY', 
  dev = 'Dev', 
  value = 'GPE')

tri.os <- tri.inc - tri.pd

tri.pd

asl17 %>% 
  dplyr::filter(DY <= 1997) %>% 
  select(AY, Dev, PdLoss) %>% 
  tidyr::spread(key = Dev, value = PdLoss)

## MackChainLadder <- function (
##   Triangle,
##   weights = 1,
##   alpha = 1,
##   est.sigma = "log-linear",
##   tail = FALSE,
##   tail.se = NULL,
##   tail.sigma = NULL,
##   mse.method = "Mack") {...}

mcl <- MackChainLadder(tri.inc)
print(mcl)

plot(mcl, which = 1)

plot(mcl, which = 2)

plot(mcl, which = 3)

plot(mcl, which = 4)

plot(mcl, which = 5)

plot(mcl, which = 6)

plot(tri.inc)

# Simple average of the dev ratios
mcl0 <- MackChainLadder(tri.inc, alpha = 0)

# Chain Ladder ratio ("loss wtd average")
mcl1 <- MackChainLadder(tri.inc, alpha = 1)

# Same as `lm` with no intercept
mcl2 <- MackChainLadder(tri.inc, alpha = 2)

x <- tri.inc[, 1:9]
y <- tri.inc[, 2:10]
x[is.na(x)] <- 0
y[is.na(y)] <- 0

beta0 <- numeric()
beta1 <- numeric()
for (i in 1:8) {
  beta0[i] <- lm(
    y[1:(10-i), i] ~ 0 + x[1:(10-i), i])$coefficient[1]
  
  beta1[i] <- lm(
    y[1:(10-i), i] ~ 1 + x[1:(10-i), i])$coefficient[2]
}

print(beta0)
print(beta1)

MackChainLadder(tri.inc)$tail
MackChainLadder(tri.inc, tail = 1.1)$tail
MackChainLadder(tri.inc, tail = TRUE)$tail

w <- matrix(1, nrow = 10, ncol = 10)
w[3, 4] <- 0

MackChainLadder(tri.inc)$f
MackChainLadder(tri.inc, weights = w)$f

## MunichChainLadder <- function(
##   Paid,
##   Incurred,
##   est.sigmaP = "log-linear",
##   est.sigmaI = "log-linear",
##   tailP = FALSE,
##   tailI = FALSE
## ) {...}

munich <- MunichChainLadder(tri.pd, tri.inc)
print(munich)

plot(munich)
