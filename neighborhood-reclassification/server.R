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
library(randomcoloR)
library(leaflet)

### Loading data
load("../data/sales_features_2017.RData")

### This line takes the most time leading to slow intial load
census_tracts <- readOGR("../data/shapefiles/2010 Census Tracts/geo_export_4d4ca7d0-0c46-467e-8dee-99c93361f914.shp")

census_pop <- read_xlsx("../data/2010census_population.xlsx",skip=7,
                        col_names=c("borough","county_code","borough_code",
                                    "2010_tract","pop_2000","pop_2010",
                                    "change","pct_change","acres","pop_per_acre"))

census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)

## NTA Shapefile
pumas <- readOGR("../data/shapefiles/Public Use Microdata Areas (PUMA)/geo_export_112df737-99d9-4599-8357-4c0b1e37faeb.shp")
# ntas <- spTransform(ntas,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
#### subsetting tracts to cluster
tracts_to_exclude <- c(census_pop$boro_ct201[census_pop$pop_2010 <= 500],"1023802") ##Rosevelt Island
# 
reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]
sales_features <- sales_features[!(sales_features$boro_ct201 %in% tracts_to_exclude),]

### subsetting features to cluster on (to be moved into function based on input)
# features_to_use <- !grepl("year|lon|lat|boro_ct201",colnames(sales_features))


# Define server logic required to draw a histogram
function(input, output) {
  
  user_selection <- eventReactive(input$select,{
    paste0(input$feature_selection,collapse = "|")
  })
  
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
    set.seed(1234)
    colours <- distinctColorPalette(K)
    tree <- hclustgeo(D0,D1,alpha=0.1)
    clusters <- data.frame(cl=cutree(tree,K))
    clusters$boro_ct201 <- sales_features$boro_ct201
    clusters <- left_join(data.frame(boro_ct201=as.character(census_tracts$boro_ct201)),
                          clusters,by="boro_ct201")
    # clusters$cl[is.na(clusters$cl)] <- K+1
    colours <- c(colours,"#A9A9A9A9") ## grey for excluded tracts
    census_tracts@data$cl <- clusters$cl
    pal <- colorFactor(distinctColorPalette(K),clusters$cl,na.color = "#A9A9A9A9")
    census_tracts
  })

  
  ##### Regular Map #####
  
  output$distPlot <- renderPlot({
  
    plot(clus_res(),col=colours[clus_res()$cl])
    
  })
  
  ##### Interactive Map #####
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(-73.885,40.72,11) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id= "mapbox.dark",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
      addPolygons(data=clus_res(),
        fillColor = pal(clus_res()$cl),
        weight = 0.5,
        opacity = 0.25,
        color = "white",
        fillOpacity = 0.8) %>%
      addPolygons(data=pumas, 
                  fillColor = "black",
                  weight = 0.75,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.1)
      
  })
  
}
