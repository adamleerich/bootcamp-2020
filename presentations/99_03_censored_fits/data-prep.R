
library(tidyverse)
library(clipr)
library(digest)
library(alrtools)



exposures_path <- 
  'C:/Users/richad/OneDrive - Beazley Group/Desktop/epl-for-r-training.csv'


exp_cols <- cols(
  ExposureYOA = col_double(),
  ExposureReference = col_character(),
  BeazleyCat = col_character(),
  Category = col_character(),
  DateOfLoss = col_date(format = ""),
  ClaimMadeDate = col_date(format = ""),
  ExposureOpenedDate = col_date(format = ""),
  LatestExposureClosedDate = col_date(format = ""),
  LimitInUSD = col_double(),
  ExcessInUSD = col_double(),
  DeductibleInUSD = col_double(),
  SettlementCurrency = col_character(),
  SlipOrderFeesPaid = col_double(),
  SlipOrderIndemnityPaid = col_double(),
  SlipOrderDefencePaid = col_double(),
  SlipOrderTotalPaid = col_double()
)




epl <- readr::read_csv(exposures_path, col_types = exp_cols)
names(epl) %>% clipr::write_clip()
dim(epl)



# Remove duplicated Exposure References
dupes <- c("BEAZL100005037382-01", "BEAZL100005025845-01")

epl2 <- epl %>%
  filter(!ExposureReference %in% dupes)


dim(epl)
dim(epl2)


hash <- character()
for (i in 1:length(epl2$ExposureReference)) {
  hash[i] <- digest(epl2$ExposureReference[i], algo="md5", serialize=F)
}


epl2$ExposureReference <- left(hash, 6)


epl2



epl2 %>% 
  rename(
    
  )