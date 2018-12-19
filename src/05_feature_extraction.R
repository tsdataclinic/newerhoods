### Feature extraction
library(dplyr)
library(stringr)
library(rgdal)
library(rgeos)
library(GISTools)

### Loading all the data

load("data/cleaned_sales_data.RData")
load(file = "./data/bbl_coords.RData")
census_tracts <- readOGR("data/shapefiles/2010 Census Tracts/geo_export_4d4ca7d0-0c46-467e-8dee-99c93361f914.shp")

###### Aggregate metrics by year and tract 
## Avg price per sq. ft
## Median price per sq. ft
## Std. Dev price per sq. ft

agg_sales <- sales_data %>% group_by(year,boro_ct201) %>% summarise(avg_price = mean(price_per_sqft),
                                                                    sd_price = sd(price_per_sqft),
                                                                    med_price = median(price_per_sqft),
                                                                    counts = n())

### Merging across all years and tracts

years <- seq(min(sales_data$year),max(sales_data$year),by=1)
tracts <- census_tracts$boro_ct201

combos <- expand.grid(years,tracts)
colnames(combos) <- c("year","boro_ct201")
combos$boro_ct201 <- as.character(combos$boro_ct201)

agg_sales <- left_join(combos,agg_sales,by=c("year","boro_ct201"))
agg_sales[is.na(agg_sales)] <- 0


### Features by year              
## Avg: 1y, 3y, 5y
## Median: 1y, 3y, 5y
## sd: 1y ,3y, 5y   
## n: 1y, 3y, 5y (number of sales)


get_moving_features_for_year <- function(sales_data,for_year,windows){
  
  features <- data.frame(boro_ct201=unique(sales_data$boro_ct201))
  features$boro_ct201 <- as.character(features$boro_ct201)
  for(i in c(1:length(windows))){
    data <- sales_data %>% filter(year >= (for_year - windows[i] + 1)) %>%
      group_by(boro_ct201) %>% summarise(avg_price = mean(price_per_sqft),
                                         sd_price = sd(price_per_sqft),
                                         med_price = median(price_per_sqft),
                                         n_sales = n())
    colnames(data) <- c(colnames(data)[1],paste0(colnames(data)[-1],"_",windows[i],"y"))
    features <- left_join(features,data,by="boro_ct201")
  }
  features[is.na(features)] <- 0
  features$year <- for_year
  return(features)
}

sales_features <- get_moving_features_for_year(sales_data,for_year=2017,windows=c(1,3,5))


### Features from MapPLUTO Data
#### N buildings
#### Total Residential units
#### Total Residential Area
#### Average age of buildings

bbl_coords$UnitsRes <- as.numeric(bbl_coords$UnitsRes)
bbl_coords$ResArea <- as.numeric(bbl_coords$ResArea)
bbl_coords$YearBuilt[bbl_coords$YearBuilt == 0] <- NA ## Removing 0 YearBuilt

bbl_coords$boro_ct201 <- paste0(bbl_coords$boro,str_pad(bbl_coords$Tract2010,6,side="right",pad="0"))
overall_features <- bbl_coords %>% group_by(boro_ct201) %>% summarise(n_bldgs = n(), 
                                                  res_units = sum(UnitsRes), 
                                                  res_area=sum(ResArea),
                                                  bldg_age = mean((2017 - YearBuilt),na.rm=TRUE))


sales_features <- merge(sales_features,overall_features,by="boro_ct201")

## Merging with complete tracts list
tract_centroids <- as.data.frame(gCentroid(census_tracts,byid=TRUE))
tract_centroids$boro_ct201 <- as.character(census_tracts@data$boro_ct201)
colnames(tract_centroids)[1:2] <- c("lon","lat")

sales_features <- left_join(tract_centroids,sales_features,by="boro_ct201")
sales_features[is.na(sales_features)] <- 0
sales_features$year[sales_features$year == 0] <- 2017


save(sales_features,file="./data/sales_features_2017.RData")

