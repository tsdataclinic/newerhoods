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
modal_features <- function(){
  bs_modal(
    id = "modal_features",
    title="Understanding Features",
      body= includeMarkdown("markdowns/features.md"),
    size="medium"
  )
}

modal_plots <- function(){
  bs_modal(
    id = "modal_plots",
    title="Interpreting Plots",
    body= includeMarkdown("markdowns/plots.md"),
    size="medium"
  )
}

### Inputs
input_housing <- function(){
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
}

input_crime <- function(){
  pickerInput(inputId = 'crime_features',#label=h6("Crime"),
              choices=list("Violations"="violation_rate",
                           "Felonies"="felony_rate",
                           "Misdemeanors"="misdemeanor_rate"),
              options=list(`actions-box`=TRUE,title="Crime Characteristics"),
              multiple=TRUE)
}

input_noise <- function(){
  pickerInput(inputId = 'call_features',#label=h6("311 Complaints"),
              choices=list("Ice Cream truck"="icecream_rate",
                           "Barking Dog"="animal_rate",
                           "Loud Music/party"="party_rate"),
              options=list(`actions-box`=TRUE,title="311 Noise Complaints"),
              multiple=TRUE)
}

input_clusters <- function(){
  sliderInput("num_clusters",
              label="Number of NewerHoods",
              min = 5,
              max = 200,
              value = 100)
}

input_plot_type <- function(){
  radioGroupButtons(inputId = "plot_type",label="Plot type",
                    choices = list("Cluster Map"="cluster_map","Heatmap"="heat_map"),
                    justified = TRUE,status="primary"
  ) %>%
  shinyInput_label_embed(
    shiny_iconlink() %>%
      bs_attach_modal(id_modal = "modal_plots")
  )
}

input_baseline <- function(){
  selectInput('baseline',label='Compare against',
              choices=list("Community Districts (59)"="cds",
                           "Public use Microdata Areas (55)"="pumas",
                           "Neighborhood Tabulation Areas (195)"="ntas",
                           "Police Precincts (77)"="precincts",
                           "School Districts (33)"="school_dists"))
}

# UI
ui <- function(request){
    tagList(
      navbarPage(

      theme = shinytheme("cerulean"),

      "NewerHoods",

      tabPanel("Map",

               ## add modals
               modal_features(),
               modal_plots(),

               ## Sidebar
               sidebarPanel(
                 input_housing(),
                 input_crime(),
                 input_noise(),
                 actionButton("select","Apply",class="btn-primary"),
                 bsTooltip("select", "Click to select or update features to be used for clustering",
                           "right", options = list(container = "body")),
                 br(),
                 br(),
                 input_clusters(),
                 input_plot_type(),
                 input_baseline(),
                 downloadButton("downloadGeoJSON","GeoJSON"),
                 downloadButton("downloadPNG","png"),
                 bookmarkButton()
               ),
               mainPanel(
                 withSpinner(leafletOutput("map", height = "535"),type=5)
                 # leafletOutput("map", height = "535")
               )),
      tabPanel("Help", includeMarkdown("markdowns/tutorial.md")),
      tabPanel("About",includeMarkdown("markdowns/intro.md"),width=4),

      # activate tooltips, popovers
      use_bs_tooltip(),
      use_bs_popover()

      )
    )
}
