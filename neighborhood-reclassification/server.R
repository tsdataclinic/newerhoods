#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

require(shiny)
require(shinyWidgets)
require(shinyjs)
require(cluster)
require(rgdal)
require(fpc)
require(dplyr)
require(ClustGeo)
require(maptools)
require(sp)
require(spdep)
require(leaflet)
require(readxl)
require(htmltools)

source("support_functions.R")

### Loading data
load("../data/sales_features_2017.RData")
load("../data/crime_rates.RData")
load("../data/nyc311_rates.RData")
# nyc311_rates
# sales_features <- merge(sales_features,crime_rates,by="boro_ct201")

features <- left_join(sales_features,crime_rates,by="boro_ct201")
features <- left_join(features,nyc311_rates,by="boro_ct201")

### This line takes the most time leading to slow intial load
# census_tracts <- readOGR("../data/shapefiles/2010 Census Tracts/geo_export_4d4ca7d0-0c46-467e-8dee-99c93361f914.shp",stringsAsFactors = FALSE)

load("../data/census_tracts.RData")
census_tracts$boro_code <- as.integer(census_tracts$boro_code)
census_pop <- read_xlsx("../data/2010census_population.xlsx",skip=7,
                        col_names=c("borough","county_code","borough_code",
                                    "2010_tract","pop_2000","pop_2010",
                                    "change","pct_change","acres","pop_per_acre"))

census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)
census_tracts <- merge(census_tracts,census_pop[,c("boro_ct201","pop_2010")],by="boro_ct201")
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
features <- features[!(features$boro_ct201 %in% tracts_to_exclude),]

function(input, output) {
  
  observe({
    if (is.null(input$housing_features) && is.null(input$crime_features) && is.null(input$call_features)) {
      shinyjs::disable("select")
    } else {
      shinyjs::enable("select")
    }
  })
  
  user_selection <- eventReactive(input$select,{
    paste0(c(input$crime_features,input$housing_features,input$call_features),collapse = "|")}
    ,ignoreNULL = TRUE) # change to false for initial load
  
  clus_res <- reactive({
    
    ### Subset data based on user selection of features
    # user_selection <- paste0(input$feature_selection,collapse = "|")
    features_to_use <- grepl(user_selection(),colnames(features))
    feature_set <- unlist(strsplit(user_selection(),"\\|"))
    plot_type <- input$plot_type
  
    ### Clustering the data based on selected features
    D0 <- dist(scale(features[,features_to_use]))
    
    list.nb <- poly2nb(reduced_tracts)
    A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)
    diag(A) <- 1
    D1 <- as.dist(1-A)
    
    K <- input$num_clusters
    # set.seed(1234)
    # colours <- distinctColorPalette(K)
    tree <- hclustgeo(D0,D1,alpha=0.2)
    clusters <- data.frame(cl=cutree(tree,K))
    
    cluster_vals <- cbind(clusters,features[,features_to_use])
    # cluster_vals <- left_join(cluster_vals,census_tracts@data[,c("boro_ct201","pop_2010")],by="boro_ct201")
    cluster_vals <- cluster_vals %>% group_by(cl) %>% summarise_if(is.numeric,mean)
    
    colnames(cluster_vals) <- c("cl",paste0(feature_set,"_mean"))
    cluster_vals$dist <- eucd_dist(cluster_vals)
    clusters <- left_join(clusters,cluster_vals,by="cl")

    clusters$boro_ct201 <- as.character(features$boro_ct201)
    clusters <- left_join(data.frame(boro_ct201=census_tracts$boro_ct201,
                                     stringsAsFactors = FALSE),
                          clusters,by="boro_ct201")
    
    
    census_tracts@data$cl <- clusters$cl
    census_tracts <- census_tracts[order(census_tracts$boro_ct201),]
    
    newerhoods <- unionSpatialPolygons(census_tracts,census_tracts$cl)
    
    census.df <- as(census_tracts,"data.frame")
    newerhoods.df <- clusters %>% group_by(cl) %>% summarise_if(is.numeric,mean)
    newerhoods.df <- newerhoods.df[!is.na(newerhoods.df$cl),]
    
    newerhoods <- SpatialPolygonsDataFrame(newerhoods,newerhoods.df)
    
    if(plot_type=="heat_map"){
      heatmap_palette <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
      pal_heatmap <- colorQuantile(heatmap_palette,newerhoods$dist,n=length(heatmap_palette),na.color = "#A9A9A9A9")
      newerhoods$colour <- pal_heatmap(newerhoods$dist)
    }else{
      map_cols <- c('#b7e882','#f4f062','#8fe2b4','#237de0','#8dcd5c','#a327ad','#d30000','#f9158d','#44b244','#2d5ead','#e8a0ea','#3ec8ed','#ea0a0a','#ef3fca','#efe8ab','#d87430')
      pal <- colorNumeric(map_cols,c(0:(length(map_cols)-1)),na.color = "#A9A9A9A9")
      newerhoods$colour <- pal(newerhoods$cl%%length(map_cols))
    }
    
    # heatmap_palette <- c('#b35806','#f1a340','#fee0b6','#f7f7f7','#d8daeb','#998ec3','#542788')
    
    # heatmap_palette <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
    # pal_heatmap <- colorQuantile(heatmap_palette,clusters$dist,n=length(heatmap_palette),na.color = "#A9A9A9A9")
    # census_tracts@data$hm_cols <- pal_heatmap(clusters[match(census_tracts$boro_ct201,clusters$boro_ct201),"dist"])
    # 
    # census_tracts@data$ordered_cl <- census_tracts$cl
    #     
        
    # for(b in c(1:5)){
        #   ct <- census_tracts[census_tracts$boro_code == b & is.na(census_tracts$ordered_cl),]
        #   unique_cl <- unique(ct$cl)
        #   unique_cl <- unique_cl[!is.na(unique_cl)]
        #   # census_tracts@data$ordered_cl[census_tracts$boro_code == b] <- match(ct$cl,unique_cl)
        #   census_tracts@data$ordered_cl[census_tracts$cl %in% unique_cl] <-
        #     match(census_tracts$cl[census_tracts$cl %in% unique_cl],unique_cl)
        # }
    
    # map_cols <- c('#b7e882','#f4f062','#8fe2b4','#237de0','#8dcd5c','#a327ad','#d30000','#f9158d','#44b244','#2d5ead','#e8a0ea','#3ec8ed','#ea0a0a','#ef3fca','#efe8ab','#d87430')
    # 
    # pal <- colorNumeric(map_cols,c(0:(length(map_cols)-1)),na.color = "#A9A9A9A9")
    # census_tracts$cl_cols <- pal(census_tracts@data$ordered_cl%%length(map_cols))
    # # }
    # if(plot_type=="heat_map"){
    #   census_tracts$colour <- census_tracts$hm_cols
    # }else{
    #   census_tracts$colour <- census_tracts$cl_cols
    # }
    # 
    # census_tracts 
    
    newerhoods
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
      addPolygons(data=census_tracts, 
                  fillColor = "black",
                  weight = 0.5,
                  opacity = 0.5,
                  color = "white",
                  fillOpacity = 0.01) %>%
      addPolygons(data=clus_res(),
                  fillColor = clus_res()$colour,
                  weight = 0.5,
                  opacity = 0.5,
                  color = "white",
                  fillOpacity = 0.6,
                  highlight = highlightOptions(
                    weight = 3*(input$plot_type == "cluster_map"),
                    color = "#a9a9a9",
                    opacity = 0.8*(input$plot_type == "cluster_map"),
                    fillOpacity = 0.6,
                    bringToFront = as.logical(TRUE*(input$plot_type == "cluster_map"))))  %>%
      addPolylines(data=baseline_map(), 
                   stroke=TRUE,
                   weight=2,
                   opacity=0.8,
                   color="white",
                   dashArray="3")})
  
  # observe({
  #   leafletProxy("map", data = clus_res()) %>%
  #     clearShapes() %>% 
  #     addPolygons(data=census_tracts, 
  #                 fillColor = "black",
  #                 weight = 0.5,
  #                 opacity = 0.5,
  #                 color = "white",
  #                 fillOpacity = 0.01) %>%
  #     addPolygons(data=clus_res(),
  #       fillColor = clus_res()$colour,
  #       weight = 0.5,
  #       opacity = 0.5,
  #       color = "white",
  #       fillOpacity = 0.6,
  #       highlight = highlightOptions(
  #         weight = 3*(input$plot_type == "cluster_map"),
  #         color = "#a9a9a9",
  #         opacity = 0.8*(input$plot_type == "cluster_map"),
  #         fillOpacity = 0.6,
  #         bringToFront = as.logical(TRUE*(input$plot_type == "cluster_map"))))  %>%
  #     addPolylines(data=baseline_map(), 
  #                  stroke=TRUE,
  #                  weight=2,
  #                  opacity=0.8,
  #                  color="white",
  #                  dashArray="3") })
  # 
  
  observe({
    proxy <- leafletProxy("map")
    heatmap_palette <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
    if(input$plot_type == "heat_map"){
      proxy %>% addLegend(position="bottomright",
                          colors = heatmap_palette,
                          labels = c("Low","","","","","","High"),
                          opacity = 1)
    }else{
        proxy %>% clearControls()
      }
       })
  
  
}
