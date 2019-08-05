points_to_feature <- function(df,col1,col2,colname){
  ## function to take a data frame with lat and lon columns and convert to a rate of
  ## counts for each census tract.
  ## Spatial columns need to be lat and lon, transformations not supported as yet. 
  
  ## creating the spatial dataframe
  colnames(df[,c(col1,col2)]) <- c("latitude","longitude")
  
  df <- df[!is.na(df$latitude) | !is.na(df$longitude),]
  coordinates(df) <- ~ longitude + latitude
  
  load("newerhoods/clean_data/census_tracts.RData")
  proj4string(df) <- proj4string(census_tracts)
  
  ## getting counts for each tract
  res <- over(df, census_tracts)
  df_rates <- as.data.frame(table(res$boro_ct201),stringsAsFactors = FALSE)
  colnames(df_rates) <- c("boro_ct201","count")

  ## loading population to get rates
  census_pop <- read_xlsx("./data/2010census_population.xlsx",skip=7,
                          col_names=c("borough","county_code","borough_code",
                                      "2010_tract","pop_2000","pop_2010",
                                      "change","pct_change","acres","pop_per_acre"))
  
  census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)
  
  df_rates <- merge(df_rates,census_pop,by="boro_ct201")
  df_rates$var_rate <- df_rates$count*1000/df_rates$pop_2010

  df_rates <- df_rates[df_rates$pop_2010 >= 500,]
  df_rates <- merge(census_tracts@data,df_rates,by="boro_ct201",all.x=TRUE)
  df_rates <- df_rates[,c("boro_ct201","var_rate")]
  df_rates$var_rate[is.na(df_rates$var_rate)] <- 0
  colnames(df_rates) <- c("boro_ct201",colname)
  return(df_rates)
}


point_data_to_feature_columns <- function(df,lat,lon,cols=NULL){
  ## function to take a data frame with lat and lon columns and convert to features
  ## of rates based on population and area using counts and sum of selected columns
  ## also included the median value for each tracts of the column(s) selected 
  ## Spatial columns need to be lat and lon, transformations not supported as yet. 
  
  ## creating the spatial dataframe
  colnames(df[,c(lat,lon)]) <- c("latitude","longitude")
  
  df <- df[!is.na(df$latitude) | !is.na(df$longitude),]
  coordinates(df) <- ~ longitude + latitude
  
  # print(getwd())
  load("clean_data/census_tracts.RData")
  proj4string(df) <- proj4string(census_tracts)
  
  ## projecting tracts to get areas (EPSG 3395 used to get accurate measures in square meters)
  census_tracts_projected <- spTransform(census_tracts,CRS("+init=epsg:3395"))
  tract_area <- data.frame(boro_ct201=census_tracts_projected$boro_ct201,tract_area=area(census_tracts_projected))
  
  ## loading population to get rates
  census_pop <- read_xlsx("clean_data/2010census_population.xlsx",skip=7,
                          col_names=c("borough","county_code","borough_code",
                                      "2010_tract","pop_2000","pop_2010",
                                      "change","pct_change","acres","pop_per_acre"))
  
  census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)
  census_pop <- as.data.frame(census_pop[,c("boro_ct201","pop_2010")])
  
  ## merging observations with tracts
  res <- over(df, census_tracts)

  if(!is.null(cols)){
    cols_to_merge <- as.data.frame(df[,cols])
    res <- cbind(res$boro_ct201,cols_to_merge)
    colnames(res) <- c("boro_ct201",cols,c("latitude","longitude"))
  }
  
  res <- res[,!(colnames(res) %in% c("latitude","longitude"))]
  res_count <- res %>% group_by(boro_ct201) %>% 
    summarise(count=n())
  
  if(!is.null(cols)){
    colnames(res) <- c("boro_ct201",cols)  
    
    res_aggs <- res %>% group_by(boro_ct201) %>% 
      summarise_all(funs(sum=sum,median=median)) %>% 
      left_join(res_count,by="boro_ct201") 
    
    res_aggs <- as.data.frame(res_aggs)
    colnames(res_aggs) <- c("boro_ct201",paste0(cols,rep(c("_sum","_median"),each=length(cols))),"count")
  }else{
    res_aggs <- as.data.frame(res_count)
  }
  
  df_rates <- merge(census_pop,res_aggs,by="boro_ct201",all.x=TRUE)
  df_rates <- merge(df_rates,tract_area,by="boro_ct201",all.x=TRUE)
  df_rates <- df_rates[df_rates$pop_2010 >= 500,]
  df_rates[is.na(df_rates)] <- 0
  # df_rates <- merge(census_tracts@data,df_rates,by="boro_ct201",all.x=TRUE)
  
  
  # df_rates <- as.data.frame(table(res$boro_ct201),stringsAsFactors = FALSE)
  # colnames(df_rates) <- c("boro_ct201","count")
  
  sum_cols <- colnames(df_rates)[grepl("_sum$",colnames(df_rates))]
  df_rates$rate_by_pop <- df_rates$count*1000/df_rates$pop_2010 ## rate per 1000 people
  df_rates$rate_by_area <- df_rates$count*1000000/(2.59*df_rates$tract_area) ## rate per sq. mile
  
  if(length(sum_cols) > 0){
    for(i in c(1:length(sum_cols))){
      df_rates[,gsub("_sum$","_rate_by_pop",sum_cols[i])] <- df_rates[,sum_cols[i]]*1000/df_rates$pop_2010 
      df_rates[, gsub("_sum$","_rate_by_area",sum_cols[i])] <- df_rates[,sum_cols[i]]*1000000/(2.59*df_rates$tract_area) ## rate per sq. mile
    }  
  }
  
  df_rates <- df_rates[,!grepl("_sum$|pop_2010$|tract_area",colnames(df_rates))]
  return(df_rates)
}

eucd_dist <- function(df){
  ## Calculates distance between points in a dataframe and 
  ## the reference point which is the vector of the minum values of each dimension
  
  df[,-1] <- scale(df[,-1])
  dist <- rep(0,dim(df)[1])
  for(i in c(2:dim(df)[2])){
    dist <- dist + (df[,i] - min(df[,i]))^2
  }
  dist <- sqrt(dist)
  return(dist[,1])
}


get_labels <- function(df){

  stats_selected <- colnames(df)[grepl("_mean",colnames(df))]
  stats_names <- gsub("_mean","",stats_selected)
  excluded_stats <- c("sd_price_1y","sd_price_3y","sd_price_5y")
  
  stats_names <- stats_names[!(stats_names %in% excluded_stats)]
  
  pretty_names <- c("<strong>2017 Median Sale Price: </strong> $%g /sq.ft.",
                    "<strong>2015-17 Median Sale Price: </strong> $%g /sq.ft.",
                    "<strong>2013-17 Median Sale Price: </strong> $%g /sq.ft.",
                    "<strong># Residential units: </strong> %g ",
                    "<strong>Building Age: </strong> %g years",
                    "<strong>Violations: </strong> %g /1000 people",
                    "<strong>Felonies: </strong> %g /1000 people",
                    "<strong>Misdemeanors: </strong> %g /1000 people",
                    "<strong>Noisy Ice Cream Trucks: </strong> %g /1000 people",
                    "<strong>Barking Dogs: </strong> %g /1000 people",
                    "<strong>Loud Music/Parties: </strong> %g /1000 people")
  
  all_stats <- c("med_price_1y","med_price_3y","med_price_5y","res_units","bldg_age",
  "violation_rate","felony_rate","misdemeanor_rate","icecream_rate","animal_rate","party_rate")
  
  ## matching columns with existing ones
  matched_columns <- match(stats_names,all_stats)
  # matched_columns <- matched_columns[!is.na(matched_columns)]
  label_names <- rep(NA,length(matched_columns))
  label_names[!is.na(matched_columns)] <- pretty_names[matched_columns[!is.na(matched_columns)]]
  label_names[is.na(matched_columns)] <- get_pretty_names(gsub("USER_","",stats_names[is.na(matched_columns)]),
                                                          type="legend")
    
  # paste0("<strong>",gsub("USER_","",stats_names[is.na(matched_columns)]),": </strong> %g")
  
  stat_columns <- paste0(stats_names,"_mean")
  
  content <- matrix(NA,nrow=dim(df)[1],ncol=length(matched_columns))
  for(i in c(1:length(matched_columns))){
    content[,i] <- sprintf(label_names[i],
                           round(unlist(df[,stat_columns[i]]),2))
  }
  
  labels <- apply(content,1,FUN= function(x) {paste(x,collapse="<br/>")})
  labels <- paste("<strong>Cluster averages</strong><br/>",labels)
  return(labels)
  
}

shiny_data <- function(){
  ### Loading data
  load("newerhoods/clean_data/sales_features_2017.RData")
  load("newerhoods/clean_data/crime_rates.RData")
  load("newerhoods/clean_data/nyc311_rates.RData")
  load("newerhoods/clean_data/census_tracts.RData")
  census_pop <- read_xlsx("newerhoods/clean_data/2010census_population.xlsx",skip=7,
                          col_names=c("borough","county_code","borough_code",
                                      "2010_tract","pop_2000","pop_2010",
                                      "change","pct_change","acres","pop_per_acre"))
  
  census_tracts$boro_code <- as.integer(census_tracts$boro_code)
  census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)
  
  census_tracts <- merge(census_tracts,census_pop[,c("boro_ct201","pop_2010")],by="boro_ct201")
  
  features <- left_join(sales_features,crime_rates,by="boro_ct201")
  features <- left_join(features,nyc311_rates,by="boro_ct201")
  features <- left_join(features,census_pop[,c("boro_ct201","pop_2010")],by="boro_ct201")
  
  #### subsetting tracts to cluster
  tracts_to_exclude <- c(census_pop$boro_ct201[census_pop$pop_2010 <= 500]) ##Rosevelt Island ,"1023802"
  reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]
  
  ## distances between centroids of tracts
  tract_centroids <- gCentroid(reduced_tracts,byid=TRUE)
  D1c <- dist(tract_centroids@coords)
  
  ### excluding tracts with 0 neighbors
  # list.nb <- poly2nb(reduced_tracts)
  # A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)
  # zero_neighbors <- reduced_tracts$boro_ct201[as.integer(which(rowSums(A) == 0,arr.ind = TRUE))]
  # tracts_to_exclude <- c(tracts_to_exclude,zero_neighbors)
  # reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]
  # 
  features <- features[!(features$boro_ct201 %in% tracts_to_exclude),]
  
  list.nb <- poly2nb(reduced_tracts)
  A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)

  diag(A) <- 1
  D1a <- as.dist(1-A)
  D1 <- (1+D1c) * D1a
  
  save(features,D1,census_tracts,reduced_tracts,file="newerhoods/clean_data/pre_compiled_data.RData")
}

get_pretty_names <- function(feature_names,type){
  
  x <- gsub("_median|rate_by_pop$|rate_by_area$|count","",feature_names)
  x <- tolower(trimws(gsub("_"," ",x)))
  
  median <- grepl("_median",feature_names)
  count <- grepl("count",feature_names)
  rate_pop <- grepl("rate_by_pop",feature_names)
  rate_area <- grepl("rate_by_area",feature_names)
  
  ## Checkbox display
  ## X_median -> Median X
  ## count -> Count
  ## rate_by_pop -> Rate (by pop)
  ## rate_by_area -> Rate (by area)
  ## X_rate_by_pop -> Rate of X (by pop)
  ## X_rate_by_area -> Rate of X (by area)
  ## X -> X
  
  checkbox_names <- x
  checkbox_names[median] <- str_c("Median ",checkbox_names[median])
  checkbox_names[count] <- "Count"
  checkbox_names[rate_pop & grepl("[:alnum:]",x)] <- str_c("Rate of ",
                                                           checkbox_names[rate_pop & grepl("[:alnum:]",x)],
                                                           " (by pop)")
  checkbox_names[rate_area & grepl("[:alnum:]",x)] <- str_c("Rate of ",
                                                           checkbox_names[rate_area & grepl("[:alnum:]",x)],
                                                           " (by area)")
  checkbox_names[rate_pop & !grepl("[:alnum:]",x)] <- "Rate (by pop)"
  checkbox_names[rate_area & !grepl("[:alnum:]",x)] <- "Rate (by area)"
  checkbox_names[!(median | count | rate_pop | rate_area)] <- paste(toupper(substr(checkbox_names[!(median | count | rate_pop | rate_area)], 1, 1)), 
                                                                    substr(checkbox_names[!(median | count | rate_pop | rate_area)], 2, nchar(checkbox_names[!(median | count | rate_pop | rate_area)])), sep="")
                                                           
  ### Legend
  ## X_median -> Median X
  ## count -> Count
  ## rate_by_pop -> Rate / 1000 people
  ## rate_by_area -> Rate / sq. mile
  ## X_rate_by_pop -> Rate of X / 1000 people
  ## X_rate_by_area -> Rate of X / sq. mile
  ## X -> X
  
  legend_names <- x
  legend_names[median] <- str_c("<strong>Median ",legend_names[median],": </strong> %g")
  legend_names[count] <- "<strong>Count: </strong> %g"
  legend_names[rate_pop & grepl("[:alnum:]",x)] <- str_c("<strong>Rate of ",
                                                           legend_names[rate_pop & grepl("[:alnum:]",x)],
                                                           ": </strong> %g /1000 people")
  legend_names[rate_area & grepl("[:alnum:]",x)] <- str_c("<strong>Rate of ",
                                                            legend_names[rate_area & grepl("[:alnum:]",x)],
                                                            ": </strong> %g /sq. mile")
  legend_names[rate_pop & !grepl("[:alnum:]",x)] <- "<strong>Rate: </strong> %g /1000 people"
  legend_names[rate_area & !grepl("[:alnum:]",x)] <- "<strong>Rate: </strong> %g /sq. mile"
  legend_names[!(median | count | rate_pop | rate_area)] <- paste("<strong>",
                                                                  toupper(substr(checkbox_names[!(median | count | rate_pop | rate_area)], 1, 1)), 
                                                                  substr(checkbox_names[!(median | count | rate_pop | rate_area)], 2, nchar(checkbox_names[!(median | count | rate_pop | rate_area)])),
                                                                  ": </strong> %g",sep="")
  if(type=="checkbox"){
    return(checkbox_names)
  }else{
    return(legend_names)
  }
}
