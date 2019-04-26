library(dbscan)

##k-means
cluster_data <- features[,features_to_use]
cluster_data <- scale(cluster_data)
cluster_km <- kmeans(cluster_data,centers = 100)
cluster_dbscan <- dbscan(cluster_data,eps = 0.15,minPts = 4)
cluster_dbscan <- dbscan(cluster_data,eps = 5,minPts = 4)
kNNdistplot(cluster_data,k=4)
abline(h=0.4,col=2)
quantile(kNNdist(cluster_data,k=8))

cl <- cluster_dbscan$cluster + 1
# cl <- cluster_km$cluster

cluster_vals <- data.frame(cbind(cl,cluster_data[,-c(1:2)],features$pop_2010))
colnames(cluster_vals) <- c("cl",feature_set,"pop_2010")
cluster_vals$boro_ct201 <- features$boro_ct201

cluster_pop <- cluster_vals %>% group_by(cl) %>% summarise(pop = sum(pop_2010),n = n())

## Going from rates back to total cases to make cluster aggregation easier
if(sum(grepl("rate",feature_set)) > 0){
  cluster_vals[,grepl("rate",colnames(cluster_vals))] <- cluster_vals[,grepl("rate",colnames(cluster_vals))]*cluster_vals$pop_2010/1000  
}

## Summarising over each cluster and calculating mean statistics
cluster_vals <- cluster_vals %>% group_by(cl) %>% summarise_if(is.numeric,mean)
colnames(cluster_vals) <- c("cl",paste0(c(feature_set,"pop"),"_mean"))
cluster_vals <- left_join(cluster_vals,cluster_pop,by="cl")

## for rate variables, dividing over the cluster population to get an overall average
if(sum(grepl("rate",feature_set)) > 0){
  cluster_vals[,grepl("rate",colnames(cluster_vals))] <- cluster_vals[,grepl("rate",colnames(cluster_vals))]*cluster_vals$n*1000/cluster_vals$pop
}

cluster_vals <- cluster_vals[,c("cl",paste0(feature_set,"_mean"))]

clusters <- data.frame(cl=cl)

## merging with cluster results
clusters <- left_join(clusters,cluster_vals,by="cl")
clusters$boro_ct201 <- as.character(features$boro_ct201)
clusters <- left_join(data.frame(boro_ct201=census_tracts$boro_ct201,
                                 stringsAsFactors = FALSE),
                      clusters,by="boro_ct201")

census_tracts@data$cl <- clusters$cl
census_tracts <- census_tracts[order(census_tracts$boro_ct201),]

## merged polygon
newerhoods <- unionSpatialPolygons(census_tracts,census_tracts$cl)

## adding cluster data
census_df <- as(census_tracts,"data.frame")
newerhoods_df <- clusters %>% group_by(cl) %>% summarise_if(is.numeric,mean)
newerhoods_df <- newerhoods_df[!is.na(newerhoods_df$cl),]
newerhoods_df$labels <-  get_labels(newerhoods_df)

newerhoods <- SpatialPolygonsDataFrame(newerhoods,newerhoods_df)
map_cols <- c('#b7e882','#f4f062','#8fe2b4',
              '#237de0','#8dcd5c','#a327ad',
              '#d30000','#f9158d','#44b244',
              '#2d5ead','#e8a0ea','#3ec8ed',
              '#ea0a0a','#ef3fca','#efe8ab','#d87430')

pal <- colorNumeric(map_cols,
                    c(0:(length(map_cols)-1)),
                    na.color = "#A9A9A9A9")
newerhoods$colour <- pal(newerhoods$cl%%length(map_cols))


map_plot <- leaflet() %>%
    setView(-73.885,40.71,11) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id= "mapbox.dark",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))
    ) %>% 
    addPolygons(data=newerhoods,
                fillColor = newerhoods$colour,
                weight = 0.5,
                opacity = 0.5,
                color = "white",
                fillOpacity = 0.6,
                ## highights
                highlight = highlightOptions(
                  weight = 4,
                  color = "orange",
                  opacity = 0.8,
                  fillOpacity = 0.6,
                  bringToFront = F),
                ## labels
                label = newerhoods$labels %>% lapply(HTML),
                labelOptions = labelOptions(
                  style = list("font-weight"="normal",
                               padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) 


map_plot
