#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(cluster)
library(rgdal)
library(fpc)
library(dplyr)
library(ClustGeo)
library(sp)
library(spdep)
library(randomcoloR)
library(leaflet)
library(readxl)
library(htmltools)

### Loading data
load("../data/sales_features_2017.RData")
load("../data/crime_rates_17.RData")
# 
# sales_features <- merge(sales_features,crime_rates,by="boro_ct201")

sales_features <- left_join(sales_features,crime_rates,by="boro_ct201")

### This line takes the most time leading to slow intial load
# census_tracts <- readOGR("../data/shapefiles/2010 Census Tracts/geo_export_4d4ca7d0-0c46-467e-8dee-99c93361f914.shp",stringsAsFactors = FALSE)

load("../data/census_tracts.RData")
census_tracts$boro_code <- as.integer(census_tracts$boro_code)
census_pop <- read_xlsx("../data/2010census_population.xlsx",skip=7,
                        col_names=c("borough","county_code","borough_code",
                                    "2010_tract","pop_2000","pop_2010",
                                    "change","pct_change","acres","pop_per_acre"))

census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)

## NTA Shapefile
# baseline_map <- readOGR("../data/shapefiles/Public Use Microdata Areas (PUMA)/geo_export_112df737-99d9-4599-8357-4c0b1e37faeb.shp")
# ntas <- readOGR("../data/shapefiles/nynta_18d/nynta.shp")
# ntas <- spTransform(ntas,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
# 
# cds <- readOGR("data/shapefiles/nycd_18d/nycd.shp")
# cds <- spTransform(cds,CRS("+proj=longlat +ellps=WGS84 +no_defs"))

#### subsetting tracts to cluster
tracts_to_exclude <- c(census_pop$boro_ct201[census_pop$pop_2010 <= 500],"1023802") ##Rosevelt Island
# 
reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]
sales_features <- sales_features[!(sales_features$boro_ct201 %in% tracts_to_exclude),]

### subsetting features to cluster on (to be moved into function based on input)
# features_to_use <- !grepl("year|lon|lat|boro_ct201",colnames(sales_features))

# ['#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061']

map_cols <- palette(c('#7f3b08','#b35806','#e08214','#fdb863','#fee0b6','#f7f7f7','#d8daeb','#b2abd2','#8073ac','#542788','#2d004b'))

function(input, output) {
  
  
  user_selection <- eventReactive(input$select,{
    paste0(input$feature_selection,collapse = "|")},ignoreNULL = FALSE)
  
  clus_res <- reactive({
    
    ### Subset data based on user selection of features
    # user_selection <- paste0(input$feature_selection,collapse = "|")
    features_to_use <- grepl(user_selection(),colnames(sales_features))
    
    ### Clustering the data based on selected features
    D0 <- dist(scale(sales_features[,features_to_use]))
    
    list.nb <- poly2nb(reduced_tracts)
    A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)
    diag(A) <- 1
    D1 <- as.dist(1-A)
    
    K <- input$num_clusters
    # set.seed(1234)
    # colours <- distinctColorPalette(K)
    tree <- hclustgeo(D0,D1,alpha=0.1)
    clusters <- data.frame(cl=cutree(tree,K))
    
    # cluster_vals <- cbind(clusters,sales_features[,features_to_use])
    # cluster_vals <- cluster_vals %>% group_by(cl) %>% summarise_if(is.numeric,mean)
    # clusters <- left_join(clusters,cluster_vals,by="cl")
    # 
    clusters$boro_ct201 <- as.character(sales_features$boro_ct201)
    clusters <- left_join(data.frame(boro_ct201=census_tracts$boro_ct201,
                                     stringsAsFactors = FALSE),
                          clusters,by="boro_ct201")
    
    census_tracts@data$cl <- clusters$cl
    
    census_tracts <- census_tracts[order(census_tracts$boro_ct201),]
    # census_tracts@data$ordered_cl <- census_tracts$cl
    
    # pal <- colorNumeric(map_cols,c(0:(length(map_cols)-1)),na.color = "#A9A9A9A9")
    
    if(K<12){
      census_tracts@data$ordered_cl <- census_tracts$cl
      pal <- colorNumeric(map_cols[ceiling(seq(1,length(map_cols),by=length(map_cols)/K))],c(1:K),na.color = "#A9A9A9A9")
    }else{
      census_tracts@data$ordered_cl <- NA

      for(b in c(1:5)){
        ct <- census_tracts[census_tracts$boro_code == b & is.na(census_tracts$ordered_cl),]
        unique_cl <- unique(ct$cl)
        unique_cl <- unique_cl[!is.na(unique_cl)]
        # census_tracts@data$ordered_cl[census_tracts$boro_code == b] <- match(ct$cl,unique_cl)
        census_tracts@data$ordered_cl[census_tracts$cl %in% unique_cl] <-
          match(census_tracts$cl[census_tracts$cl %in% unique_cl],unique_cl)
      }

      pal <- colorNumeric(map_cols,c(0:(length(map_cols)-1)),na.color = "#A9A9A9A9")
    }

    census_tracts@data$cl_cols <- pal(census_tracts@data$ordered_cl%%length(map_cols))
    census_tracts
  })


  ### Baseline Map
  
  baseline_map <- eventReactive(input$baseline,{
    load(file=paste0("../data/",input$baseline,".RData"))
    get(input$baseline)
    })

  
  ##### Interactive Map #####
    
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(-73.885,40.71,10) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id= "mapbox.dark",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(data=clus_res(),
        fillColor = clus_res()$cl_cols,
        weight = 0.5,
        opacity = 0.25,
        color = "white",
        fillOpacity = 0.8) %>%
      addPolygons(data=baseline_map(), 
                  fillColor = "black",
                  weight = 0.75,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.01)
      
  })
  
}
