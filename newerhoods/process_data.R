library(igraph)

source("support_functions.R")

### Construct pre-merged rds files for the datasets of interest from a collection of local files
shiny_data <- function(){
  
  ## Load data files
  load("newerhoods/data/features/interim/sales_features_2017.RData")
  load("newerhoods/data/features/interim/crime_rates.RData")
  load("newerhoods/data/features/interim/nyc311_rates.RData")
  
  
  
  ## Load census files
  load("newerhoods/data/census/census_tracts.RData")
  census_pop <- read_xlsx("newerhoods/data/census/2010census_population.xlsx",skip=7,
                          col_names=c("borough","county_code","borough_code",
                                      "2010_tract","pop_2000","pop_2010",
                                      "change","pct_change","acres","pop_per_acre"))
  
  census_tracts$boro_code <- as.integer(census_tracts$boro_code)
  census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)
  
  census_tracts <- merge(census_tracts,census_pop[,c("boro_ct201","pop_2010")],by="boro_ct201")
  
  ##### subsetting tracts to cluster
  tracts_to_exclude <- c(census_pop$boro_ct201[census_pop$pop_2010 <= 500]) ##Rosevelt Island ,"1023802"
  reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]
  
  ## Merge data sets with census
  features <- left_join(sales_features,crime_rates,by="boro_ct201")
  features <- left_join(features,nyc311_rates,by="boro_ct201")
  features <- left_join(features,census_pop[,c("boro_ct201","pop_2010")],by="boro_ct201")
  
  
  
  ## Pre-compute D1 for clustering
  tract_centroids <- gCentroid(reduced_tracts,byid=TRUE)
  D1c <- dist(tract_centroids@coords) ## euclidean distance between tract centroids
  
  features <- features[!(features$boro_ct201 %in% tracts_to_exclude),]
  
  list.nb <- poly2nb(reduced_tracts)
  ## adjecency between tracts, 1 if tracts adjescent, 0 if not
  A <- nb2mat(list.nb,style = "B",zero.policy = TRUE) 
  
  
  ## graph based D1
  # g <- graph_from_adjacency_matrix(as.matrix(D1c)*A,mode = "undirected",weighted=TRUE)
  # D1g <- distances(g,mode="all")
  # D1g[is.infinite(D1g)] <- as.matrix(D1c)[is.infinite(D1g)]
  # 
  ## complementing of the adjecency matrix, 1 if tracts not adjacent, 0 if they are
  diag(A) <- 1 
  D1a <- as.dist(1-A) 
  ## multiplying euclidean distance with complement of the adjecency to get a composite measure
  ## such that adjacent tracts have a distance of 0, and rest have distance 1 + their euclidean dist
  D1 <- (1+D1c) * D1a
  # D1 <- as.dist(D1g) *D1a
  
  save(features,D1,census_tracts,reduced_tracts,file="newerhoods/data/features/processed/pre_compiled_data.RData")
}


process_baseline_maps <- function(){
  ### check for all shapefiles in raw data in boundaries directory
  source_folder <- "data/boundaries/raw/"
  shape_files <- list.files(source_folder,pattern = "*.shp$",recursive = TRUE)
  info_json <- jsonlite::fromJSON("data/boundaries/info.json")
  info_df <- info_json$maps 
  
  folders <- gsub("\\/(.*)","",shape_files) ## extract folder names
  info_df$slug <- slugify(info_df$name) ## slugify names
  
  if(length(shape_files) != nrow(info_df)){
    stop("Number of shapefiles and info don't match")
  }
  
  ## load shapefiles abd save as .rds with slug name
  for(i in c(1:length(shape_files))){
    shape_data <- readOGR(paste0(source_folder,shape_files[i]),stringsAsFactors = FALSE)
    file_name <- info_df$slug[info_df$folder == folders[i]]
    shape_data <- spTransform(shape_data,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    saveRDS(shape_data,file = paste0("data/boundaries/processed/",file_name,".rds"))
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
  slug_text <- gsub("[[:space:]]","-",trimws(gsub("[[:punct:]]","",tolower(t))))
  return(slug_text)
}


