#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

## Data handling packages
require(dplyr)
require(readxl)

## UI/UX packages
require(shiny)
require(shinyWidgets)
require(shinyjs)
require(leaflet)
require(htmltools)

## Spatial packages
require(rgdal)
require(maptools)
require(sp)
require(spdep)

## clustering
require(cluster)
require(fpc)
require(ClustGeo)

source("support_functions.R")

## function to add legend to plot
add_legend <- function(plot_type){
  proxy <- leafletProxy("map")
  heatmap_palette <- c('#ffffb2','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')
  if(plot_type == "heat_map"){
    proxy %>% addLegend(position="bottomright",
                        colors = heatmap_palette,
                        labels = c("Low","","","","","","High"),
                        opacity = 1)
  }else{
    proxy %>% clearControls()}
}

### Loading data
load("clean_data/sales_features_2017.RData")
load("clean_data/crime_rates.RData")
load("clean_data/nyc311_rates.RData")
load("clean_data/census_tracts.RData")
census_pop <- read_xlsx("clean_data//2010census_population.xlsx",skip=7,
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

## Server
function(input, output) {
  
  ## To Add: error handling on clicking select without selecting any features
  
  # observe({
  #   if (is.null(input$housing_features) && is.null(input$crime_features) && is.null(input$call_features)) {
  #     shinyjs::disable("select")
  #   } else {
  #     shinyjs::enable("select")
  #   }
  # })
  # 
  
  user_selection <- eventReactive(input$select,{
    paste0(c(input$crime_features,input$housing_features,input$call_features),collapse = "|")}
    ,ignoreNULL = TRUE) # change to false for initial load
  
  num_clus <- reactive({
    input$num_clusters
  }) %>% debounce(1000)
  
  clus_res <- reactive({
    
    # c("icecream_rate","animal_rate","party_rate")
    ### Subset data based on user selection of features
    features_to_use <- grepl(user_selection(),colnames(features))
    feature_set <- unlist(strsplit(user_selection(),"\\|"))
    
    ### Clustering the data based on selected features
    D0 <- dist(scale(features[,features_to_use]))
    
    list.nb <- poly2nb(reduced_tracts)
    A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)
    diag(A) <- 1
    D1 <- as.dist(1-A)
    
    K <- num_clus()
    set.seed(1729)
    
    tree <- hclustgeo(D0,D1,alpha=0.15)
    clusters <- data.frame(cl=cutree(tree,K))
    
    ## generating cluster average statistics (weighted mean for rates)
    
    ## Calculating total population and number of tracts for each cluster
    cluster_vals <- cbind(clusters,features[,feature_set],features$pop_2010)
    colnames(cluster_vals) <- c("cl",feature_set,"pop_2010")
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
    
    ## distance between clusters
    cluster_vals$dist <- eucd_dist(cluster_vals)
    
    ## merging with cluster results
    clusters <- left_join(clusters,cluster_vals,by="cl")
    clusters$boro_ct201 <- as.character(features$boro_ct201)
    clusters <- left_join(data.frame(boro_ct201=census_tracts$boro_ct201,
                                     stringsAsFactors = FALSE),
                          clusters,by="boro_ct201")
    
    ## adding to census tracts to merge polygons
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
    
    ## Plotting operations
    plot_type <- input$plot_type
    
    if(plot_type=="heat_map"){
      heatmap_palette <- c('#ffffb2','#fed976','#feb24c',
                           '#fd8d3c','#fc4e2a','#e31a1c','#b10026')
      pal_heatmap <- colorQuantile(heatmap_palette,newerhoods$dist,
                                   n=length(heatmap_palette),
                                   na.color = "#A9A9A9A9")
      newerhoods$colour <- pal_heatmap(newerhoods$dist)
    }else{
      map_cols <- c('#b7e882','#f4f062','#8fe2b4',
                    '#237de0','#8dcd5c','#a327ad',
                    '#d30000','#f9158d','#44b244',
                    '#2d5ead','#e8a0ea','#3ec8ed',
                    '#ea0a0a','#ef3fca','#efe8ab','#d87430')
      
      pal <- colorNumeric(map_cols,
                          c(0:(length(map_cols)-1)),
                          na.color = "#A9A9A9A9")
      newerhoods$colour <- pal(newerhoods$cl%%length(map_cols))
    }
    
    return(newerhoods)
  })
  
  ## To Do: Option to not have any baseline map
  ### Baseline Map
  baseline_map <- eventReactive(input$baseline,{
    load(file=paste0("clean_data//",input$baseline,".RData"))
    get(input$baseline)
  })
  
  
  ##### Interactive Map #####
  output$map <- 
    renderLeaflet({
      leaflet() %>%
        setView(-73.885,40.71,11) %>%
        addProviderTiles("MapBox", options = providerTileOptions(
          id= "mapbox.dark",
          accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))
        ) %>% 
        addPolygons(data=census_tracts, 
                    fillColor = "black",
                    weight = 0.3,
                    opacity = 0.3,
                    color = "white",
                    fillOpacity = 0.01
        ) %>%
        addPolygons(data=clus_res(),
                    fillColor = clus_res()$colour,
                    weight = 0.5,
                    opacity = 0.5,
                    color = "white",
                    fillOpacity = 0.6,
                    ## highights
                    highlight = highlightOptions(
                      weight = 4,
                      color = ifelse(input$plot_type == "cluster_map","orange","green"),
                      opacity = 0.8,
                      fillOpacity = 0.6,
                      bringToFront = F),
                    ## labels
                    label = clus_res()$labels %>% lapply(HTML),
                    labelOptions = labelOptions(
                      style = list("font-weight"="normal",
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        addPolylines(data=baseline_map(), 
                     stroke=TRUE,
                     weight=2.75,
                     opacity=0.75,
                     color="white",
                     dashArray="4")
    })
  
  ## To do: Simplify this. Feels like too many observes
  observeEvent(input$select,{
    add_legend(input$plot_type)
  })
  
  observe({
    add_legend(input$plot_type)
  })
  
  observeEvent(input$num_clusters,{
    add_legend(input$plot_type)
  })
  
}
