require(devtools)
install.packages('lattice')
devtools::install_version("rgeos",version ='0.3-28')
### packages needed for the App

packages <- c("dplyr","readxl","shiny","shinyWidgets","shinyjs",
              "leaflet","htmltools","shinyBS","shinythemes","markdown",'geojson',
              "shinycssloaders","rgdal","maptools","sp","spdep",'mapview',
              "cluster","fpc","ClustGeo")

## installing required packages
install.packages(packages,quiet = FALSE)

## installing required packages
if (!require(gpclib)) {
  install.packages("gpclib", type="source")
  require(gpclib)
  #gcplib::gpclibPermit()
}

webshot::install_phantomjs()

## installing the dev version of bsplus from GitHub 
devtools::install_github("ijlyttle/bsplus")



