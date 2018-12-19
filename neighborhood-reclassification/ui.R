#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(markdown)

# Define UI for application that draws a histogram
tagList(
  navbarPage(
  theme = shinytheme("paper"),
  "OpenDataProject",
  tabPanel("Introduction",includeMarkdown("intro.md")),
  tabPanel("Clustering",
           sidebarPanel(
             sliderInput("num_clusters",
                         label=h5("Number of clusters"),
                         min = 5,
                         max = 100,
                         value = 55),
             checkboxGroupInput("feature_selection",label=h5("Features to use"),
                                choices=list("1y Sale Price per sq. ft"="avg_price_1y|sd_price_1y",
                                             "3y Sale Price per sq. ft"="avg_price_3y|sd_price_3y",
                                             "5y Sale Price per sq. ft"="avg_price_5y|sd_price_5y",
                                             "No. of Residential units"="res_units",
                                             "Avg. age of buildings"="age"),
                                selected = "avg_price_1y|sd_price_1y"),
             actionButton("select","Update Features",class="btn-primary")
           ),
           mainPanel(
             leafletOutput("map")
             #plotOutput("distPlot")
           )),
  tabPanel("Similarity","Coming Soon: Find Similar neighborhoods")
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
