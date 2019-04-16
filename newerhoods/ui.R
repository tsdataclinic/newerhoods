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
source("components.R")

# UI
bootstrapPage(
  theme = "custom.css",
  title = "NewerHoods",
  header_nav,
  div(
    class="main col-sm-10",
    div(
      class="sidebar",
      div(class="orange-border", ""),
      div(
        class="well",
        ## add modals
        modal_features,
        modal_plots,
        info,
        input_housing,
        input_housing_sales,
        input_crime,
        input_noise,
        actionButton("select","Apply",class="btn-custom")
      ),
      withTags({
        div(
          class="flex",
          span(class='link', 'Getting started'),
          ul(
            class="links flex", 
            li(class="link", a(href="/", "Intro video")),
            li(class="link", a(href="/", "Credits"))
          )
        )
      })
    ),
    div(
      class="map-content",
      div(class="blue-border", ""),
      div(
        class="map custom",
        withSpinner(leafletOutput("map", height = "535"),type=5),
        map_control_panel
      ),
      bsTooltip("select", "Click to select or update features to be used for clustering",
                          "right", options = list(container = "body")),
      input_baseline
    )
  ),
  # activate tooltips, popovers
  use_bs_tooltip(),
  use_bs_popover()
)


