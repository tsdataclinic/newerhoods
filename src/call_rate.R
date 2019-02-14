### 311 data to features
library("RSocrata")
library(readxl)


nyc311_bees <- read.socrata(
  "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$where=complaint_type='Harboring Bees/Wasps'",
  #where=created_date>='2018-01-01T00:00:00.000'",
  app_token = "G1jXll4MFa1CUAPu4EgNdxL9K"
)

nyc311_animals <- read.socrata(
  "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$where=complaint_type like '%25Illegal Animal%25'",
  #where=created_date>='2018-01-01T00:00:00.000'",
  app_token = "G1jXll4MFa1CUAPu4EgNdxL9K"
)

nyc311_animals <- rbind(nyc311_animals,nyc311_bees)

nyc311_rates <- points_to_feature(nyc311_animals,"latitude","longitude")
colnames(nyc311_rates) <- c("boro_ct201","call_rate")
save(nyc311_rates,file="./data/nyc311_animals.RData")
