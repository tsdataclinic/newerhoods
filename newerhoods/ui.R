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
require(shinycssloaders)

### Definitions

### Modals
modal_features <- 
  bs_modal(
    id = "modal_features",
    title="Understanding Features",
    body= includeMarkdown("markdowns/features.md"),
    size="medium"
  )

modal_plots <- 
  bs_modal(
    id = "modal_plots",
    title="Interpreting Plots",
    body= includeMarkdown("markdowns/plots.md"),
    size="medium"
  )

### Inputs
input_housing <- 
  pickerInput(inputId = 'housing_features',label="Features to use",
              choices=list("2017 Median Sale Price"="med_price_1y|sd_price_1y",
                           "2015-17 Median Sale Price"="med_price_3y|sd_price_3y",
                           "2013-17 Median Sale Price"="med_price_5y|sd_price_5y",
                           #"No. of Residential units"="res_units",
                           "Age of buildings"="bldg_age"),
              options=list(`actions-box`=TRUE,title="Housing Characteristics"),
              multiple=TRUE, selected = "med_price_1y|sd_price_1y"
  ) %>% 
  shinyInput_label_embed(
    shiny_iconlink() %>%
      bs_attach_modal(id_modal = "modal_features")
  )

input_crime <- 
  pickerInput(inputId = 'crime_features',#label=h6("Crime"),
              choices=list("Violations"="violation_rate",
                           "Felonies"="felony_rate",
                           "Misdemeanors"="misdemeanor_rate"),
              options=list(`actions-box`=TRUE,title="Crime Characteristics"),
              multiple=TRUE)

input_noise <- 
  pickerInput(inputId = 'call_features',#label=h6("311 Complaints"),
              choices=list("Ice Cream truck"="icecream_rate",
                           "Barking Dog"="animal_rate",
                           "Loud Music/party"="party_rate"),
              options=list(`actions-box`=TRUE,title="311 Noise Complaints"),
              multiple=TRUE)

input_clusters <- 
  sliderInput("num_clusters",
              label="Number of NewerHoods",
              min = 5,
              max = 200,
              value = 100)

input_plot_type <- 
  radioGroupButtons(inputId = "plot_type",label="Plot type",
                    choices = list("Cluster Map"="cluster_map","Heatmap"="heat_map"),
                    justified = TRUE,status="primary"
  ) %>%
  shinyInput_label_embed(
    shiny_iconlink() %>%
      bs_attach_modal(id_modal = "modal_plots")
  )

input_baseline <- 
  selectInput('baseline',label='Compare against',
              choices=list("Community Districts (59)"="cds",
                           "Public use Microdata Areas (55)"="pumas",
                           "Neighborhood Tabulation Areas (195)"="ntas",
                           "Police Precincts (77)"="precincts",
                           "School Districts (33)"="school_dists"))


# UI
tagList(
  navbarPage(
    
    theme = shinytheme("cerulean"),
    
    "NewerHoods",
    
    tabPanel("Map",
             
             ## add modals
             
             modal_features,
             modal_plots,
             
             ## Sidebar
             
             sidebarPanel(
               materialSwitch(
                 inputId = "upload",
                 label = "Upload data?",
                 value = FALSE,
                 status = "primary"
               ),
               conditionalPanel(
                 condition = "input.upload",
                 fileInput("file1","Choose a CSV File",
                           multiple = FALSE,
                           accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
               ),
               
               # Select Geographic id
               conditionalPanel(
                 condition = "input.upload",
                 radioButtons(
                   inputId = "geo",
                   label = "Select Geographic Identifier", 
                   choices = c("Latitude & Longitude" = "lat_lon",
                               "Borough & Census Tract" = "boro_tract",
                               "Combined Tract ID" = "boro_ct"),
                   selected = NULL,
                   inline = FALSE)),
               
               # Select lat/lon columns
               conditionalPanel(
                 condition = "input.geo == lat_lon",
                 selectInput("lat","Select Latitude column",
                             choices = NULL, multiple = FALSE),
                 selectInput("lat","Select Longitude column",
                             choices = NULL, multiple = FALSE)),
               
               # Select boto/ct columns
               conditionalPanel(
                 condition = "input.geo == boro_tract",
                 selectInput("boro","Select Borough column",
                             choices = NULL, multiple = FALSE),
                 selectInput("ct","Select Tract column",
                             choices = NULL, multiple = FALSE)),
               
               # Select boro_ct columns
               conditionalPanel(
                 condition = "input.geo == boro_ct",
                 selectInput("boro_ct","Select Borough column",
                             choices = NULL, multiple = FALSE)),
               
               input_housing,
               input_crime,
               input_noise,
               
               actionButton("select","Redraw",class="btn-primary"),
               bsTooltip("select", "Click to select or update features to be used for clustering",
                         "right", options = list(container = "body")),
               tags$hr(),
               input_clusters,
               input_plot_type,
               input_baseline
             ),
             mainPanel(
               withSpinner(leafletOutput("map", height = "535"),type=3,color.background = "white")
               # leafletOutput("map", height = "535")
             )),
    tabPanel("Help", includeMarkdown("markdowns/tutorial.md")),
    tabPanel("About",includeMarkdown("markdowns/intro.md"),width=4),
    
    # activate tooltips, popovers
    use_bs_tooltip(),
    use_bs_accordion_sidebar(),
    use_bs_popover()
    
  )
)


