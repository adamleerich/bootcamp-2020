source('common.R')

## library(tidyverse)
## dracula <- readr::read_file(
##   'http://gutenberg.org/cache/epub/345/pg345.txt')
## dracula_words <- strsplit(dracula, split = '\\s+')

library(tidyverse)
dracula <- readr::read_file('dracula.txt')
dracula_words <- strsplit(dracula, split = '\\s+')

words <- unlist(dracula_words)
words_lower <- tolower(words)
nchar_words <- nchar(words_lower)
nchar_words10 <- pmin(nchar_words, 10)
table_words10 <- table(nchar_words10)
barplot(table_words10, names.arg = c(1:9, '10+'))

## unlist(dracula_words)
## tolower(unlist(dracula_words))
## nchar(tolower(unlist(dracula_words)))
## pmin(nchar(tolower(unlist(dracula_words))), 10)
## table(pmin(nchar(tolower(unlist(dracula_words))), 10))
## 
## # Fully nested!
## barplot(table(pmin(nchar(tolower(unlist(dracula_words))), 10)), names.arg = c(1:9, '10+'))

dracula_words %>% 
  unlist() %>% 
  tolower() %>% 
  nchar() %>% 
  pmin(10) %>% 
  table() %>% 
  barplot(names.arg = c(1:9, '10+'))

library(raw)
data("MultiTri")

## str(MultiTri)
## head(MultiTri)

financials <- MultiTri %>% 
  dplyr::select(
    CumulativePaid, CumulativeIncurred, IBNR)

no_lag <- MultiTri %>% 
  dplyr::select(-Lag)

years <- MultiTri %>% 
  dplyr::select(contains("year"))

new_tri <- MultiTri %>% 
  rename(DevelopmentLag = Lag)

new_tri <- new_tri %>% 
  mutate(
    PaidToIncurred = CumulativePaid / CumulativeIncurred, 
    Upper = DevelopmentYear <= 1997)

new_tri <- MultiTri %>% 
  mutate(Upper = DevelopmentYear <= 1997) %>% 
  dplyr::select(-DevelopmentYear)

## MultiTri %>%
##   arrange(AccidentYear)
## 
## MultiTri %>%
##   arrange(desc(IBNR))

upper_tri <- MultiTri %>% 
  dplyr::filter(DevelopmentYear <= 1997)

upper_tri <- MultiTri %>% 
  dplyr::filter(
    DevelopmentYear <= 1997, 
    IBNR > 500)

every_fifth <- MultiTri %>%
  slice(seq(from = 5, by = 5, to = nrow(MultiTri)))

df_grouped <- MultiTri %>% 
  group_by(Company, AccidentYear)

dfBigYear <- MultiTri %>% 
  group_by(AccidentYear) %>% 
  summarize(BiggestIBNR = max(IBNR))

dfBigCase <- MultiTri %>% 
  mutate(PaidToIncurred = CumulativePaid / CumulativeIncurred) %>% 
  dplyr::filter(PaidToIncurred < 0.4) %>% 
  group_by(Company) %>% 
  arrange(desc(PaidToIncurred)) %>% 
  slice(1) %>% 
  dplyr::select(Company, AccidentYear)

dfCo <- data.frame(
  Company = unique(MultiTri$Company), 
  stringsAsFactors = FALSE)

dfCo$PolicyHolderSurplus <- rnorm(
  nrow(dfCo), 1e8, 0.3 * 1e8)

print(dfCo)


dfJoined <- dplyr::inner_join(MultiTri, dfCo)
dfJoined %>% 
  dplyr::select(
    Company, PolicyHolderSurplus, DevelopmentYear) %>% 
  head(3)

library(tidyr)
one_co <- new_tri %>% 
  dplyr::filter(
    Company == unique(MultiTri$Company)[1], 
    Line == 'Workers Comp')

wide_tri <- one_co %>%
  dplyr::select(AccidentYear, Lag, NetEP, CumulativePaid) %>% 
  tidyr::spread(Lag, CumulativePaid)

print(wide_tri)


wide_tri <- one_co %>%
  dplyr::filter(Upper) %>% 
  dplyr::select(AccidentYear, Lag, NetEP, CumulativePaid) %>% 
  spread(Lag, CumulativePaid)
wide_tri

long_tri <- wide_tri %>% 
  tidyr::gather(Lag, CumulativePaid, -AccidentYear, -NetEP)
long_tri
