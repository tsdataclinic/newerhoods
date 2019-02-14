library("RSocrata")
library(readxl)

crime_15_17 <- read.socrata(
  "https://data.cityofnewyork.us/resource/9s4h-37hy.csv?$where=cmplnt_fr_dt>='2017-12-12T00:00:00.000'",
  app_token = "G1jXll4MFa1CUAPu4EgNdxL9K"
)

crime_15_17 <- crime_15_17[!is.na(crime_15_17$latitude) |
                             !is.na(crime_15_17$longitude) &
                             crime_15_17$law_cat_cd == "VIOLATION",]
  
coordinates(crime_15_17) <- ~ longitude + latitude

load("./data/census_tracts.RData")

proj4string(crime_15_17) <- proj4string(census_tracts)

res <- over(crime_15_17, census_tracts)
res_table <- as.data.frame(table(res$boro_ct201),stringsAsFactors = FALSE)
colnames(res_table) <- c("boro_ct201","count")

census_pop <- read_xlsx("./data/2010census_population.xlsx",skip=7,
                        col_names=c("borough","county_code","borough_code",
                                    "2010_tract","pop_2000","pop_2010",
                                    "change","pct_change","acres","pop_per_acre"))

census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)

res_table <- merge(res_table,census_pop,by="boro_ct201")
res_table$crime_rate <- res_table$count*1000/res_table$pop_2010

res_table <- res_table[res_table$pop_2010 >= 500,]
crime_rates <- res_table
crime_rates <- merge(census_tracts@data,crime_rates,by="boro_ct201",all.x=TRUE)
crime_rates <- crime_rates[,c("boro_ct201","crime_rate")]
crime_rates$crime_rate[is.na(crime_rates$crime_rate)] <- 0
save(crime_rates,file="./data/crime_rates_17.RData")
