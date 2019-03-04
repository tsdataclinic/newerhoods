### Merging Data
### Merge cleaned sales and Map Pluto data
### Use Geoclient API to get condo locations

library(httr)
library(jsonlite)

load(file="data/intermediary/missing_sales_data.RData")

BASE_URL <- 'https://api.cityofnewyork.us/geoclient/v1/'
app_id <- Sys.getenv('GEOCLIENT_API_APP_ID')
app_key <- Sys.getenv('GEOCLIENT_API_APP_KEY')

missing_sales_data$BBL <- NA
error_cases <- NULL

for(i in c(1:dim(missing_sales_data)[1])){
  if(i%%10000 == 0){print(i)}
  query <- paste0("bbl.json?borough=",missing_sales_data$borough[i],
                  "&block=",missing_sales_data$block[i],
                  "&lot=",missing_sales_data$lot[i])
  url <- paste0(BASE_URL,query,'&app_id=',app_id,'&app_key=',app_key)
  tryCatch(
    {
      res <- GET(url)
      res <- fromJSON(rawToChar(res$content))
      if(!("message" %in% names(res$bbl))){
        missing_sales_data$BBL[i] <- ifelse(res$bbl$condominiumBillingBbl == "0000000000",
                                            res$bbl$bbl,res$bbl$condominiumBillingBbl)
      }
    }, 
    error=function(cond){
      error_cases <<- append(error_cases,i)
    }
  )
}

# res <- GET(url=paste0(BASE_URL,path,'&app_id=',app_id,'&app_key=',app_key))

load("./data/intermediary/bbl_coords.RData")
merged_missing_sales_data <- merge(missing_sales_data,bbl_coords,by="BBL")

save(missing_sales_data,file="./data/intermediary/missing_sales_data_wBBL.RData")
save(merged_missing_sales_data,file="./data/intermediary/condo_sales_data_geocoded.RData")

