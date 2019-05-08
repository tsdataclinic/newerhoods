library("RSocrata")
library(readxl)

source("./newerhoods/support_functions.R")

app_token <- Sys.getenv('NYC_OPENDATA_API_TOKEN')
crime_17 <- read.socrata(
  "https://data.cityofnewyork.us/resource/9s4h-37hy.csv?$where=cmplnt_fr_dt>='2017-12-01T00:00:00.000'",
  app_token = app_token
)

crime_violations <- crime_17[crime_17$law_cat_cd == "VIOLATION",]
crime_felony <- crime_17[crime_17$law_cat_cd == "FELONY",]
crime_misdemeanor <- crime_17[crime_17$law_cat_cd == "MISDEMEANOR",]

crime_rates <- points_to_feature(crime_violations,"latitude","longitude","violation_rate")
crime_rates <- merge(crime_rates,points_to_feature(crime_felony,"latitude","longitude","felony_rate"),by="boro_ct201")
crime_rates <- merge(crime_rates,points_to_feature(crime_misdemeanor,"latitude","longitude","misdemeanor_rate"),by="boro_ct201")

save(crime_rates,file="./data/crime_rates.RData")
