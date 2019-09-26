#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)
library(shinyjs)
library(htmltools)
library(bsplus)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(markdown)
library(shinycssloaders)
library(shinyFeedback)

### Definitions
source("components.R")

# UI
ui <- function(request){
  bootstrapPage(
    # shinyjs::useShinyjs(),
    useShinyFeedback(),
    theme = "custom.css",
    title = "NewerHoods",
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Lato:300,400,400italic,500,500italic,700,700italic,900")
    ),
    header_nav,
    div(
      class="main col-xs-11",
      div(
        class="sidebar",
        div(class="orange-border", ""),
        div(
          class="well",
          ## add modals
          modal_features,
          modal_credits,
          modal_plots,
          modal_feedback,
          # modal_example,
          modal_upload,
          div(align="center",
              upload_link),
          tags$hr(),
          # upload_switch,
          info,
          input_housing(),
          div(class="radiogroup-custom",input_housing_sales()),
          lapply(as.list(feature_inputs$vars),FUN=function(x) eval(parse(text=x))),
          # input_crime(),
          # input_noise(),
          input_user_features,
          actionButton("select","Apply",class="btn-custom"),
          snackbar(
            id = "FeatureSelection",
            message = "Please select atleast one feature!")
        ),
        intro_links
      ),
      div(
        class="map-content",
        div(class="blue-border", ""),
        div(
          class="map custom",
          download_dropdown,
          # fluidRow(column(3, 
          #                 shinyjs::hidden(div(id = 'loading', 
          #                                     addSpinner(div(), spin = "fading-circle", color = "#0099a6"))))),
          
          custom_withSpinner(leafletOutput("map", height = "535"),type=3,
                      color.background = "white",color="#0099a6"),
          map_control_panel()
        ),
        bsTooltip("select", "Click to select or update features to be used for clustering",
                  "right", options = list(container = "body"))
      )
    ),
    footer,
    # activate tooltips, popovers
    use_bs_tooltip(),
    use_bs_popover()
  )
}
