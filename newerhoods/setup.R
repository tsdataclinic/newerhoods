### packages needed for the App

packages <- c("shiny","shinyWidgets","shinyjs","leaflet","shinycssloaders",
              "htmltools","shinythemes","shinyBS","markdown",
              "rgdal","maptools","sp","spdep","cluster","fpc",
              "ClustGeo","devtools","rgeos")

## installing required packages
install.packages(packages,quiet = TRUE)

## installing required packages
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()              

## installing the dev version of bsplus from GitHub 
require(devtools)
devtools::install_github("ijlyttle/bsplus")


