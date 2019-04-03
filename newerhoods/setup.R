### packages needed for the App

packages <- c("dplyr","readxl","shiny","shinyWidgets","shinyjs",
              "leaflet","htmltools","shinyBS","shinythemes","markdown",
              "shinycssloaders","rgeos","rgdal","maptools","sp","spdep",
              "cluster","fpc","ClustGeo")

## installing required packages
install.packages(packages,quiet = TRUE)

## installing required packages
if (!require(gpclib)) {
  install.packages("gpclib", type="source")
  gpclibPermit()
}

## installing the dev version of bsplus from GitHub 
require(devtools)
devtools::install_github("ijlyttle/bsplus")



