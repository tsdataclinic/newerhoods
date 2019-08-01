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
require(broom)

## UI/UX packages
require(shiny)
require(shinyWidgets)
require(shinyjs)
require(leaflet)
require(htmltools)
library(grDevices)
require(raster)
## Spatial packages

## spatial libraries with issues in deployment
# if (!require(gpclib)) install.packages("gpclib", type="source")
# gpclibPermit()

library(rgeos)
library(geojson)
require(rgdal)
require(maptools)
require(sp)
require(spdep)
library(mapview)
require(ggmap)

## clustering
require(cluster)
require(fpc)
require(ClustGeo)

source("settings_local.R")
source("support_functions.R")

## General settings
options(shiny.maxRequestSize=50*1024^2)

## function to validate input
validate_selection <- function(a,b,c){
  if(a == "" & b == "" & c == ""){
    "Please select atleast one feature"
  }
}

## loading pre-cleaned data

load(file="clean_data/pre_compiled_data.RData")
load(file="clean_data/cds.RData")
load(file="clean_data/ntas.RData")
load(file="clean_data/precincts.RData")
load(file="clean_data/pumas.RData")
load(file="clean_data/school_dists.RData")

merged_features <- features

## Server
function(input, output, session) {
  
  # Need to exclude the buttons from themselves being bookmarked
  # setBookmarkExclude("share")
  observeEvent(input$share, {
    session$doBookmark()
  })
  
  raw_user_data <- reactive({
    req(input$file)
    raw_user_df <- read.csv(input$file$datapath)
    # merged_features <<- features
    raw_user_df
  })
  
  ## updating columns to select from
  observe({
    req(input$file)
    col_names <- colnames(raw_user_data())
    updateSelectInput(session, "lat", choices= col_names)
    updateSelectInput(session, "lon", choices= col_names)
    updateSelectInput(session, "boro", choices= col_names)
    updateSelectInput(session, "ct", choices= col_names)
    updateSelectInput(session, "boro_ct", choices= col_names)
    updateSelectInput(session, "user_columns", choices= col_names)
  })
  
  user_data <- observeEvent(input$upload_done,{
    # toggleModal(session,modalId = "modal_upload",toggle="close")
    # removeModal(session,modalId = "modal_upload")
    if(input$geo == 'lat_lon'){
      ## convert points to rates fatures
      user_df <- point_data_to_feature_columns(raw_user_data(),lat=input$lat,lon=input$lon,cols=input$user_columns)
      colnames(user_df) <- paste0("USER_",colnames(user_df))
      colnames(user_df)[1] <- "boro_ct201"
    }else if(input$geo == 'boro_tract'){
      ## reformat the boro and tract columns and combine for merging
      user_df <- raw_user_data()
      user_df$boro_ct201 <- paste0(user_df[,input$boro],str_pad(user_df[,input$ct],6,side="right",pad="0"))
      user_df <- user_df %>% group_by(boro_ct201) %>% 
        dplyr::select(input$user_features) %>%
        summarise_all(funs(mean))
      user_df <- as.data.frame(user_df)
      colnames(user_df) <- c("boro_ct201",paste0("USER_",input$user_columns))
    }else{
      ## reformat the combined boro-tract column
      user_df <- raw_user_data()
      user_df$boro_ct201 <- as.character(user_df[,input$boro_ct])
      user_df <- user_df %>% dplyr::select(input$user_columns,boro_ct201) %>% 
        group_by(boro_ct201) %>% 
        summarise_all(funs(mean))
      user_df <- as.data.frame(user_df)
      colnames(user_df) <- c("boro_ct201",paste0("USER_",input$user_columns))
    }
    
    generated_feature_names <- gsub("USER_","",colnames(user_df))
    generated_feature_names <- generated_feature_names[generated_feature_names != "boro_ct201"]
    updateSelectInput(session, "user_features", choices= generated_feature_names)
    merged_features <<- left_join(features,user_df,by="boro_ct201")
    
    return(user_df)
  })
  
  
  user_selection <- eventReactive(input$select,{
    selection <- c(input$crime_features,input$housing,input$call_features)
    if("sale_price" %in% selection){
      selection <- selection[selection != "sale_price"]  
      selection <- c(selection,input$sales_features)
    }
    selection <- paste0(selection,collapse = "|")
    
    user_feature_selection <- paste0("USER_",input$user_features,collapse = "|")
    
    if(user_feature_selection != "USER_"){
      selection <- paste0(c(selection,user_feature_selection),collapse = "|")  
      selection <- gsub("^\\|","",selection)
    }
    
    validate(
      need(selection != "", "Please select at least one feature.")
    )
    selection
  }, ignoreNULL = FALSE) # change to false for initial load
  
  
  tree <- eventReactive(user_selection(),{
    
    features_to_use <- grepl(user_selection(),colnames(merged_features))
    feature_set <- unlist(strsplit(user_selection(),"\\|"))
    
    ### Clustering the data based on selected features
    D0 <- dist(scale(merged_features[,features_to_use]))
    
    set.seed(1729)
    tree <- hclustgeo(D0,D1,alpha=0.1)
    
    return(tree)
  },ignoreNULL = FALSE)
  
  clus_res <- reactive({
    ### Subset data based on user selection of features
    features_to_use <- grepl(user_selection(),colnames(merged_features))
    feature_set <- unlist(strsplit(user_selection(),"\\|"))
    
    K <- input$num_clusters
    clusters <- data.frame(cl=cutree(tree(),K))
    
    ## cluster statistics to find optimal numbers
    # avg_sil <- rep(NA,200)
    # for(i in c(5:200)){
    #   c <- cutree(tree(),i) 
    #   avg_sil[i] <- summary(silhouette(c,D))$avg.width
    # }
    # cl_metric <- data.frame(k=c(1:200),avg_sil=avg_sil)
    # cl_metric$groups <- cut(c(1:200),breaks=4)
    # 
    # s <- cl_metric %>% group_by(groups) %>% summarize(K=max(k),k_opt=min(k)+which.max(avg_sil)-1)
    # 
    
    ## generating cluster average statistics (weighted mean for rates)
    ## Calculating total population and number of tracts for each cluster
    cluster_vals <- cbind(clusters,merged_features[,feature_set],merged_features$pop_2010)
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
    
    ## distance between clusters to use for heatmap
    cluster_vals$dist <- eucd_dist(cluster_vals)
    
    ## merging with cluster results
    clusters <- left_join(clusters,cluster_vals,by="cl")
    clusters$boro_ct201 <- as.character(merged_features$boro_ct201)
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
    newerhoods_df <- as.data.frame(newerhoods_df)
    newerhoods_df <- newerhoods_df[!is.na(newerhoods_df$cl),]
    newerhoods_df$labels <-  get_labels(newerhoods_df)
    
    newerhoods <- SpatialPolygonsDataFrame(newerhoods,newerhoods_df)
    
    return(newerhoods)
  }) %>% debounce(10)
  
  ## Add the ability to download the results as GeoJSON
  output$downloadGEOJson<- downloadHandler(
    filename = function() {
      print("test")
      paste("clusters",".geojson",sep="")
    },
    content = function(file){
      print("test")
      write(as.geojson(clus_res()), file )
    }
  )
  
  output$downloadPNG<- downloadHandler(
    filename = function() {
      paste("clusters",".png",sep="")
    },
    content = function(file){
      nh <- clus_res()
      bbox <- nh@bbox
      map_dl <- get_stamenmap(bbox = c(left=bbox[1,1], bottom=bbox[2,1],
                                       right=bbox[1,2],top=bbox[2,2]),
                              maptype = "toner-lite", source = "stamen", zoom = 11)
      
      tt <- tidy(nh,region = "cl")
      tt <- as.data.frame(tt, stringsAsFactors = FALSE)
      
      p <- ggmap(map_dl) + 
        geom_polygon(data = fortify(tt),
                     aes(long, lat, group = group, fill=id),
                     colour = "white", alpha = 0.6) + 
        theme(legend.position="none") 
      ggsave(file, plot = p, device = "png")
    }
  )
  
  newerhoods <- reactive({
    
    
    enable_heatmap <- input$enable_heatmap
    
    newerhoods <- clus_res()
    
    if(enable_heatmap==TRUE){
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
                    '#0093ee','#136bb0','#1d4c7b',
                    '#1d293a','#017f7c','#015d5f','#003b3e')
      
      pal <- colorNumeric(map_cols,
                          c(0:(length(map_cols)-1)),
                          na.color = "#A9A9A9A9")
      newerhoods$colour <- pal(newerhoods$cl%%length(map_cols))
    }
    
    return(newerhoods)
  })
  
  ##### Interactive Map #####
  map_reactive <- reactive({
    leaflet() %>%
      setView(-73.885,40.71,11) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id= "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))
      ) %>%
      addPolygons(data=newerhoods(),
                  fillColor = newerhoods()$colour,
                  stroke = TRUE,
                  weight = 1,
                  opacity = 0.5,
                  color = "lightgrey",
                  fillOpacity = 0.6,
                  ## highights
                  highlight = highlightOptions(
                    weight = 4,
                    color = ifelse(input$enable_heatmap == FALSE,"orange","green"),
                    opacity = 0.8,
                    fillOpacity = 0.6,
                    bringToFront = F),
                  ## labels
                  label = newerhoods()$labels %>% lapply(HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight"="normal",
                                 padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
  })
  
  output$map <-renderLeaflet({
    map_reactive()  
  })
  
  ## To do: Simplify this. Feels like too many observes
  ## Adding Heatmap Legend
  ## function to add legend to plot
  ## function to add legend to plot
  add_legend <- function(enable_heatmap){
    proxy <- leafletProxy("map")
    # heatmap_palette <- c('#0093ee','#136bb0','#1d4c7b','#017f7c','#015d5f','#003b3e', '#1d293a')
    heatmap_palette <- c('#ffffb2','#fed976','#feb24c',
                         '#fd8d3c','#fc4e2a','#e31a1c','#b10026')
    if(enable_heatmap == TRUE){
      proxy %>% addLegend(position="bottomright",
                          colors = heatmap_palette[7:1],
                          labels = c("Low","","","","","","High")[7:1],
                          opacity = 1)
    }else{
      proxy %>% clearControls()}
  }
  
  observe({add_legend(input$enable_heatmap)})
  observeEvent(clus_res(),{add_legend(input$enable_heatmap)})
  
  
  ### Baseline Map
  baselines <- c("cds","ntas","pumas","precincts","school_dists")
  add_baselines <- function(){
    for(i in c(1:length(baselines))){
      baseline_map <- get(baselines[i])
      proxy <- leafletProxy("map")
      proxy %>% 
        addPolylines(data=baseline_map, 
                     stroke=TRUE,
                     weight=2,
                     opacity=0.75,
                     color="white",
                     dashArray="5",
                     group = baselines[i])
      if(baselines[i] != input$baseline){
        proxy %>% hideGroup(baselines[i])  
      }
    }
  }
  
  observeEvent({input$enable_heatmap},{add_baselines()})
  observeEvent({clus_res()},{add_baselines()})
  
  observeEvent(input$baseline,{
    if(input$baseline != "none"){
      baseline_map <- get(input$baseline)
      
      proxy <- leafletProxy("map")
      proxy %>% hideGroup(baselines)
      proxy %>% showGroup(input$baseline)
    }else{
      proxy <- leafletProxy("map")
      proxy %>% hideGroup(baselines)
    }
  })
}
