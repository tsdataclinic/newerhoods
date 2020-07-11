# library(HatchedPolygons)
library(sp)
library(sf)

points_to_feature <- function(df,col1,col2,colname){
  ## function to take a data frame with lat and lon columns and convert to a rate of
  ## counts for each census tract.
  ## Spatial columns need to be lat and lon, transformations not supported as yet. 
  
  ## creating the spatial dataframe
  colnames(df[,c(col1,col2)]) <- c("latitude","longitude")
  
  df <- df[!is.na(df$latitude) | !is.na(df$longitude),]
  coordinates(df) <- ~ longitude + latitude
  
  load("data/census/census_tracts.RData")
  proj4string(df) <- proj4string(census_tracts)
  
  ## getting counts for each tract
  res <- over(df, census_tracts)
  df_rates <- as.data.frame(table(res$boro_ct201),stringsAsFactors = FALSE)
  colnames(df_rates) <- c("boro_ct201","count")
  
  ## loading population to get rates
  census_pop <- read_xlsx("./data/census/2010census_population.xlsx",skip=7,
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
  lat_idx <- match(lat,colnames(df))
  lon_idx <- match(lon,colnames(df))
  colnames(df)[lat_idx] <- "latitude"
  colnames(df)[lon_idx] <- "longitude"
  # print(colnames(df[,c(lat,lon)]))
  df <- df[!is.na(df$latitude) | !is.na(df$longitude),]
  coordinates(df) <- ~ longitude + latitude
  
  # print(getwd())
  load("data/census/census_tracts.RData")
  proj4string(df) <- proj4string(census_tracts)
  
  ## projecting tracts to get areas (EPSG 3395 used to get accurate measures in square meters)
  census_tracts_projected <- spTransform(census_tracts,CRS("+init=epsg:3395"))
  tract_area <- data.frame(boro_ct201=census_tracts_projected$boro_ct201,tract_area=raster::area(census_tracts_projected))
  
  ## loading population to get rates
  census_pop <- read_xlsx("data/census/2010census_population.xlsx",skip=7,
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
  
  ### Columns defined in feature dictionary
  
  # get feature dict
  feature_info <- process_features_json()
  
  # subset to ones in stats selected
  label_df <- feature_info[feature_info$column_name %in% stats_names
                           ,c("column_name","label_html")]
  
  ### Columns from user uploaded data
  user_columns <- stats_names[grepl("USER_",stats_names)]
  if(length(user_columns) > 0){
    user_columns_legends <- get_pretty_names(gsub("USER_","",user_columns),
                                             type="legend")
    
    user_label_df <- data.frame(column_name=user_columns,label_html=user_columns_legends,
                                stringsAsFactors = FALSE)
    label_df <- rbind(label_df,user_label_df)
  }
  
  label_df$column_name <- paste0(label_df$column_name,"_mean")
  
  content <- matrix(NA,nrow=dim(df)[1],ncol=dim(label_df)[1])
  for(i in c(1:(dim(label_df)[1]))){
    content[,i] <- sprintf(label_df$label_html[i],
                           round(unlist(df[,label_df$column_name[i]]),2))
  }
  
  labels <- apply(content,1,FUN= function(x) {paste(x,collapse="<br/>")})
  labels <- paste("<strong>Cluster averages</strong><br/>",labels)
  return(labels)
  
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
  checkbox_names[rate_pop & grepl("[[:alnum:]]",x)] <- str_c("Rate of ",
                                                           checkbox_names[rate_pop & grepl("[[:alnum:]]",x)],
                                                           " (by pop)")
  checkbox_names[rate_area & grepl("[[:alnum:]]",x)] <- str_c("Rate of ",
                                                            checkbox_names[rate_area & grepl("[[:alnum:]]",x)],
                                                            " (by area)")
  checkbox_names[rate_pop & !grepl("[[:alnum:]]",x)] <- "Rate (by pop)"
  checkbox_names[rate_area & !grepl("[[:alnum:]]",x)] <- "Rate (by area)"
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
  legend_names[rate_pop & grepl("[[:alnum:]]",x)] <- str_c("<strong>Rate of ",
                                                         legend_names[rate_pop & grepl("[[:alnum:]]",x)],
                                                         ": </strong> %g /1000 people")
  legend_names[rate_area & grepl("[[:alnum:]]",x)] <- str_c("<strong>Rate of ",
                                                          legend_names[rate_area & grepl("[[:alnum:]]",x)],
                                                          ": </strong> %g /sq. mile")
  legend_names[rate_pop & !grepl("[[:alnum:]]",x)] <- "<strong>Rate: </strong> %g /1000 people"
  legend_names[rate_area & !grepl("[[:alnum:]]",x)] <- "<strong>Rate: </strong> %g /sq. mile"
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


load_baseline_maps <- function(){
  source_folder <- "data/boundaries/processed/"
  files <- list.files(source_folder,"*.rds")
  for(i in c(1:length(files))){
    tmp <- readRDS(file=paste0(source_folder,files[i]))
    assign(gsub(".rds","",files[i]),tmp)
  }
}


get_baseline_choices <- function(){
  ### check for all shapefiles in raw data in boundaries directory
  info_json <- jsonlite::fromJSON("data/boundaries/info.json")
  info_df <- info_json$maps 
  slug_names <- slugify(info_df$name)
  
  choices <- vector("list",(length(slug_names)+1))
  names(choices) <- c("None",info_df$name)
  choices[1:(length(slug_names)+1)] <- c("none",slug_names)
  
  return(choices)
}

slugify <- function(t){
  slug_text <- gsub("[[:space:]]","_",trimws(gsub("[[:punct:]]","",tolower(t))))
  return(slug_text)
}


get_homogeneity <- function(k,alpha,D0,D1,D0_sq,D1_sq,T0,T1){
  
  cl <- cutree(hclustgeo(alpha=alpha,D0,D1),k)
  C <- cl2mat(cl)
  
  Q0 <- 1 - sum(D0_sq*C)/T0
  Q1 <- 1 - sum(D1_sq*C)/T1
  
  return(list(Q0,Q1))
}

get_sil_width <- function(k,D0,res){
  c <- cutree(res,k)
  avg_sil <- summary(silhouette(c,D0))$avg.width
  return(avg_sil)
}

cl2mat <- function(cl){
  unique_cl <- unique(cl)
  n <- length(cl)
  M <- matrix(0,nrow=n,ncol=n)
  for(i in c(1:length(unique_cl))){
    idx <- which(cl==unique_cl[i])
    M[idx,idx] <- 1/(2*n*length(idx))
  }
  return(M)
}


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


get_opt <- function(alpha,gain){
  
  df <- data.frame(alpha=alpha,gain=gain)
  df <- df[order(alpha),]
  
  for(i in c(1:nrow(df))){
    if(df$gain[i] < 0){
      if(i == 1){
        return(0.1)
      }else{
        return(df$alpha[i-1])  
      }
    }
  }
  return(df$alpha[nrow(df)])
}


get_jaccard_index <- function(nh,sp,j_threshold=0.8){
  nh$area_y <- raster::area(nh)
  sp$area_x <- raster::area(sp)
  sp <- gBuffer(sp, byid=TRUE, width=0)
  sp$base_id <- as.numeric(row.names(sp@data))
  intersection <- raster::intersect(sp,nh)
  intersection$area_xy <- raster::area(intersection)
  intersection$area_xuy <- intersection$area_x + intersection$area_y - intersection$area_xy
  intersection$J_k <- intersection$area_xy/intersection$area_xuy
  jaccard_data <- intersection@data
  if("cl.1" %in% colnames(jaccard_data)){
    jaccard_data$cl <- jaccard_data$cl.1  
  }
  jaccard_data <- jaccard_data %>% group_by(base_id) %>% summarise(area=mean(area_x),J_k=max(J_k))
  jaccard_index <- sum(jaccard_data$area*jaccard_data$J_k)/sum(jaccard_data$area)
  tmp <- jaccard_data[order(jaccard_data$J_k,decreasing = TRUE),]
  print(tmp[1:5,])
  highlight_ids <- jaccard_data$base_id[jaccard_data$J_k >= j_threshold]
  highlight_areas <- sp[sp$base_id %in% highlight_ids,]
  highlight_areas_hatch<-hatched.SpatialPolygons(highlight_areas,density=250,angle=45);
  proj4string(highlight_areas_hatch)<-proj4string(highlight_areas)
  highlight_areas_hatch <- spTransform(highlight_areas_hatch, CRS("+proj=longlat +datum=WGS84"))
  
  return(list(jaccard_index=jaccard_index,highlight_area=highlight_areas,hatch=highlight_areas_hatch))
}


map_coloring <- function(x, algorithm="greedy", ncols=NA, minimize=FALSE, palette=NULL, contrast=1) {
  if (inherits(x, "Spatial")) x <- as(x, "sf")
  if (inherits(x, "sf")) {
    # get adjacency list
    adj <- get_neighbours(x)
  } else if (is.list(x)) {
    adj <- x
  } else stop("Unknown x argument")
  
  if (!is.null(palette)) {
    # reverse palette
    if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
      revPal <- function(p)rev(p)
      palette <- substr(palette, 2, nchar(palette))
    } else revPal <- function(p)p
    
    if (palette[1] %in% rownames(RColorBrewer::brewer.pal.info)) {
      if (is.na(ncols)) ncols <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
      palette2 <- revPal(suppressWarnings(get_brewer_pal(palette, ncols, contrast, plot = FALSE)))
    } else {
      if (is.na(ncols)) ncols <- length(palette)
      palette2 <- rep(palette, length.out=ncols)
    }
  } else if (is.na(ncols)) ncols <- 8
  
  k <- length(adj)
  
  if (algorithm=="greedy") {
    # list of occupied colors
    occ <- as.list(rep(0, k))
    
    # vector of output colors
    cols <- rep(NA, k)
    
    # order of degree (starting with the highest)
    ord <- order(sapply(adj, length), decreasing = TRUE)
    
    showWarn <- FALSE
    for (i in ord) {
      sel <- setdiff(1:ncols, occ[[i]])
      
      if (!length(sel)) {
        sel <- 1
        showWarn <- TRUE
      }
      z <- if (minimize) sel[1] else sample(sel, 1)
      
      for (j in ord) if (i %in% adj[[j]]) occ[[j]] <- c(occ[[j]], z)
      cols[i] <- z
    }
  } else stop("Unknown algorithm")
  
  if (showWarn) warning("Unable to color with ", ncols, " colors. Adjacent polygons may have the same color.", call. = FALSE)
  
  if (!is.null(palette)) {
    palette2[cols]
  } else {
    cols
  }
}

get_neighbours <- function(x) {
  y <- sf::st_intersects(x, x)
  n <- length(y)
  
  mapply(function(yi, i) {
    setdiff(yi, i)
  }, y, 1L:n, SIMPLIFY = FALSE)
  
}

