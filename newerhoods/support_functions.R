points_to_feature <- function(df,col1,col2,colname){
  ## function to take a data frame with lat and lon columns and convert to a rate of
  ## counts for each census tract.
  ## Spatial columns need to be lat and lon, transformations not supported as yet. 
  
  ## creating the spatial dataframe
  colnames(df[,c(col1,col2)]) <- c("latitude","longitude")
  
  df <- df[!is.na(df$latitude) | !is.na(df$longitude),]
  coordinates(df) <- ~ longitude + latitude
  
  load("./data/census_tracts.RData")
  proj4string(df) <- proj4string(census_tracts)
  
  ## gtting counts for each tract
  res <- over(df, census_tracts)
  df_rates <- as.data.frame(table(res$boro_ct201),stringsAsFactors = FALSE)
  colnames(df_rates) <- c("boro_ct201","count")

  # print(colnames(df_rates))
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
  
  matched_columns <- match(stats_names,all_stats)
  matched_columns <- matched_columns[!is.na(matched_columns)]
  
  stat_columns <- paste0(all_stats[matched_columns],"_mean")
  
  content <- matrix(NA,nrow=dim(df)[1],ncol=length(matched_columns))
  for(i in c(1:length(matched_columns))){
    content[,i] <- sprintf(pretty_names[matched_columns[i]],
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
  tracts_to_exclude <- c(census_pop$boro_ct201[census_pop$pop_2010 <= 500],"1023802") ##Rosevelt Island
  reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]
  features <- features[!(features$boro_ct201 %in% tracts_to_exclude),]
  
  list.nb <- poly2nb(reduced_tracts)
  A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)
  diag(A) <- 1
  D1 <- as.dist(1-A)
  
  save(features,D1,census_tracts,file="newerhoods/clean_data/pre_compiled_data.RData")
}

