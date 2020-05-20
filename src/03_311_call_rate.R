### 311 data to features
library("RSocrata")
library(readxl)

source("support_functions.R")
source("process_data.R")

app_token <- Sys.getenv('NYC_OPENDATA_API_TOKEN')

nyc311_complaints <- read.socrata(
  "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$where=created_date between '2015-01-01T00:00:00' and '2019-01-01T12:00:00'&$limit=100000",
  app_token = app_token)

## Noise complaints
noise_complaint_types <- unique(nyc311_complaints$complaint_type[
  grepl("noise",nyc311_complaints$complaint_type,ignore.case = TRUE)])
nyc311_complaints_minus_noise <- nyc311_complaints[!(nyc311_complaints$complaint_type 
                                                     %in% noise_complaint_types),]
## Housing-related complaints (source: Beta NYC Tenants Map)
housing_complaint_types <- unique(nyc311_complaints_minus_noise$complaint_type[
  grepl("hot water|plumbing|paint|plaster|rodent|dirty|unsanitory|elevator",
        nyc311_complaints_minus_noise$complaint_type,ignore.case = TRUE)])
nyc311_complaints_minus_hous_noise <- nyc311_complaints_minus_noise[
  !(nyc311_complaints_minus_noise$complaint_type %in% housing_complaint_types),]

## Street/sidewalk related complaints
street_complaint_types <- rownames(sort(table(
  nyc311_complaints_minus_hous_noise$complaint_type[
    grepl("street|sidewalk",nyc311_complaints_minus_hous_noise$location_type,
          ignore.case = TRUE)])/1000,decreasing = TRUE))[1:16]

rm(nyc311_complaints_minus_noise,nyc311_complaints_minus_hous_noise)

st_years <- c('2015-01-01T00:00:00','2016-01-01T00:00:00','2017-01-01T00:00:00',
              '2018-01-01T00:00:00','2019-01-01T00:00:00')
en_years <- c('2016-01-01T00:00:00','2017-01-01T00:00:00','2018-01-01T00:00:00',
              '2019-01-01T00:00:00','2019-10-01T00:00:00')

base_url <- "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$where=created_date between "

type_filter <- paste0("('",paste0(c(noise_complaint_types,
                                    housing_complaint_types,
                                    street_complaint_types),collapse = "', '"),"')")

print("Getting data from API")

nyc311_complaints <- nyc311_complaints[0,]

for(i in c(4:length(st_years))){
  tictoc::tic()
  
  time_filter <- paste0("'",st_years[i],"' and '",en_years[i],"' AND complaint_type in")
  full_url <- paste0(base_url,time_filter,type_filter)
  print(full_url)
  tmp <- read.socrata(url = full_url,
  app_token = app_token)
  nyc311_complaints <- rbind(nyc311_complaints,tmp)
  tictoc::toc()
  
}

write.csv(nyc311_complaints,file="data/features/nyc311_complaints_2015_19.csv")
save(nyc311_complaints,file="data/features/nyc311_complaints_2015_19.RData")


nyc_noise <- nyc311_complaints[nyc311_complaints$complaint_type %in% noise_complaint_types,]
nyc_housing <- nyc311_complaints[nyc311_complaints$complaint_type %in% housing_complaint_types,]
nyc_street <- nyc311_complaints[nyc311_complaints$complaint_type %in% street_complaint_types,]
nyc_street <- nyc_street[grepl("street|sidewalk",nyc_street$location_type,ignore.case = TRUE),]

nyc311_rates <- points_to_feature(nyc_noise,"latitude","longitude","noise_rate")

nyc311_rates <- merge(nyc311_rates,
                      points_to_feature(nyc_housing,"latitude","longitude","housing_rate"),
                      by="boro_ct201")

nyc311_rates <- merge(nyc311_rates,
                      points_to_feature(nyc_street,"latitude","longitude","street_rate"),
                      by="boro_ct201")

write.csv(nyc311_rates,file="data/features/transformed/nyc311_rates.csv",row.names = FALSE)

print("Updating data for Web-App")
generate_data()

# nyc311_complaints <- read.socrata(
#   "https://data.cityofnewyork.us/resource/fhrw-4uyv.csv?$where=complaint_type like '%25Noise%25'",
#   app_token = app_token)
# party_desc <- c("Loud Music/Party","Noise: Loud Music/Nighttime(Mark Date And Time) (NP1)",
#                 "Noise: Loud Music/Daytime (Mark Date And Time) (NN1)")
# animal_desc <- c("Noise, Other Animals (NR6)","Noise, Barking Dog (NR5)")
# ice_cream_desc <- c("Noise, Ice Cream Truck (NR4)")
# 
# nyc_noise_party <- nyc311_complaints[nyc311_complaints$descriptor %in% party_desc,]
# nyc_noise_animals <- nyc311_complaints[nyc311_complaints$descriptor %in% animal_desc,]
# nyc_noise_ice_cream <- nyc311_complaints[nyc311_complaints$descriptor %in% ice_cream_desc,]
# 
# nyc311_rates <- points_to_feature(nyc_noise_party,"latitude","longitude","party_rate")
# 
# nyc311_rates <- merge(nyc311_rates,
#                       points_to_feature(nyc_noise_animals,"latitude","longitude","animal_rate"),
#                       by="boro_ct201")
# 
# nyc311_rates <- merge(nyc311_rates,
#                       points_to_feature(nyc_noise_ice_cream,"latitude","longitude","icecream_rate"),
#                       by="boro_ct201")

# save(nyc311_rates,file="./data/nyc311_rates.RData")
