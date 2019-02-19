#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinyBS)
require(shinyjs)
require(htmltools)
require(bsplus)
require(shinythemes)
require(shinyWidgets)
require(leaflet)
require(markdown)

modal_features <- 
  bs_modal(
    id = "modal_features",
    title="Features",
    body= includeMarkdown("features.md"),
    size="medium"
)

tagList(
  navbarPage(
  theme = shinytheme("cerulean"),
  "NewerHoods",
  tabPanel("About",includeMarkdown("intro.md")),
  tabPanel("Clustering",
           
           ## add modals
           modal_features,
           
           
           sidebarPanel(
             ## try out selectInput(multiple=TRUE, selectize=TRUE) with option groups
             #h5("Features to use"),
             pickerInput(inputId = 'housing_features',label="Features to use",
                         choices=list("1y Sale Price per sq. ft"="med_price_1y|sd_price_1y",
                                              "3y Sale Price per sq. ft"="med_price_3y|sd_price_3y",
                                              "5y Sale Price per sq. ft"="med_price_5y|sd_price_5y",
                                              "No. of Residential units"="res_units",
                                              "Avg. age of buildings"="age"),
                         options=list(`actions-box`=TRUE,title="Housing Characteristics"),
                         multiple=TRUE#,
                         #selected = "med_price_1y|sd_price_1y"
                         ) %>% 
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_attach_modal(id_modal = "modal_features")
                   ),
             
             pickerInput(inputId = 'crime_features',#label=h6("Crime"),
                         choices=list("Crime Rates"="crime_rate"),
                         options=list(title="Crime Characteristics"),
                         multiple=TRUE),
             
             pickerInput(inputId = 'call_features',#label=h6("311 Complaints"),
                         choices=list("Complaints about animals"="call_rate"),
                         options=list(title="311 Complaints"),
                         multiple=TRUE),
             
             # checkboxGroupInput("feature_selection",label=h5("Features to use"),
             #                    choices=list("1y Sale Price per sq. ft"="med_price_1y|sd_price_1y",
             #                                 "3y Sale Price per sq. ft"="med_price_3y|sd_price_3y",
             #                                 "5y Sale Price per sq. ft"="med_price_5y|sd_price_5y",
             #                                 "No. of Residential units"="res_units",
             #                                 "Avg. age of buildings"="age",
             #                                 "Crime Rates"="crime_rate"),
             #                    selected = "med_price_1y|sd_price_1y"),
             actionButton("select","Select Features",class="btn-primary") ,
             # actionButton(,class="btn-primary"),
             bsTooltip("select", "Click to select or update features to be used for clustering",
                       "right", options = list(container = "body")),
             # p("Click to update the features"),
             
             # chooseSliderSkin("Shiny",color="#67daff"),
             # setSliderColor("DeepSkyBlue",1),
             sliderInput("num_clusters",
                         label="Number of clusters",
                         min = 5,
                         max = 200,
                         value = 55),
             
             # shinyWidgets::materialSwitch(inputId="heatmap",label="Plot Type",value=FALSE,status="primary"),
             radioGroupButtons(inputId = "plot_type",label="Plot type",
                               choices = list("Cluster Map"="cluster_map","Heatmap"="heat_map"),
                               justified = TRUE,status="primary") 
             
              %>%
               shinyInput_label_embed(
                 shiny_iconlink() %>%
                   bs_attach_modal(id_modal = "modal_features")
               ),
             
             selectInput('baseline',label='Compare against',
                         choices=list("Community Districts"="cds",
                                      "Public Use Microdata Areas (PUMA)"="pumas",
                                      "Neighborhood Tabulation Areas (NTA)"="ntas",
                                      "Police Precincts"="precincts"),
                         selected = "cds")
           ),
           mainPanel(
             leafletOutput("map",height = 565)
             #plotOutput("distPlot")
           )),
  tabPanel("Appendix",includeMarkdown("appendix.md"))
))

# fluidPage(
#   # Application title
#   titlePanel("New York City Neighborhood Reclassification"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        sliderInput("num_clusters",
#                    label=h4("Number of clusters"),
#                    min = 5,
#                    max = 100,
#                    value = 55),
#        checkboxGroupInput("feature_selection",label=h4("Features to use"),
#                           choices=list("1y Sale Price per sq. ft"="avg_price_1y|sd_price_1y",
#                                        "3y Sale Price per sq. ft"="avg_price_3y|sd_price_3y",
#                                        "5y Sale Price per sq. ft"="avg_price_5y|sd_price_5y",
#                                        "No. of Residential units"="res_units",
#                                        "Avg. age of buildings"="age"),
#                           selected = "avg_price_1y|sd_price_1y"),
#        submitButton("Run")
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#         leafletOutput("map")
#        #plotOutput("distPlot")
#     )
#   )
# )
