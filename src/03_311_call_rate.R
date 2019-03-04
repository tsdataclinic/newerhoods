### 311 data to features
library("RSocrata")
library(readxl)

source("./neighborhood-reclassification/support_functions.R")

app_token <- Sys.getenv('NYC_OPENDATA_API_TOKEN')
nyc311_complaints <- read.socrata(
  "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$where=complaint_type like '%25Noise%25'",
  app_token = app_token)

party_desc <- c("Loud Music/Party","Noise: Loud Music/Nighttime(Mark Date And Time) (NP1)",
                "Noise: Loud Music/Daytime (Mark Date And Time) (NN1)")
animal_desc <- c("Noise, Other Animals (NR6)","Noise, Barking Dog (NR5)")
ice_cream_desc <- c("Noise, Ice Cream Truck (NR4)")

nyc_noise_party <- nyc311_complaints[nyc311_complaints$descriptor %in% party_desc,]
nyc_noise_animals <- nyc311_complaints[nyc311_complaints$descriptor %in% animal_desc,]
nyc_noise_ice_cream <- nyc311_complaints[nyc311_complaints$descriptor %in% ice_cream_desc,]

nyc311_rates <- points_to_feature(nyc_noise_party,"latitude","longitude","party_rate")

nyc311_rates <- merge(nyc311_rates,
                      points_to_feature(nyc_noise_animals,"latitude","longitude","animal_rate"),
                      by="boro_ct201")

nyc311_rates <- merge(nyc311_rates,
                      points_to_feature(nyc_noise_ice_cream,"latitude","longitude","icecream_rate"),
                      by="boro_ct201")

save(nyc311_rates,file="./data/nyc311_rates.RData")
