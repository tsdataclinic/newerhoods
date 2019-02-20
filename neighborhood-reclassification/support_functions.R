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


