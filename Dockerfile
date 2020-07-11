FROM rocker/shiny:3.6.3

RUN apt-get update; apt-get install -y inotify-tools; apt-get install -y libssl-dev

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libudunits2-dev \
    netcdf-bin \
    protobuf-compiler \
    tk-dev \
    unixodbc-dev

## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
     devtools dplyr lattice readxl shinyWidgets shinyjs \
     leaflet htmltools shinyBS shinythemes geojson \
     shinycssloaders furrr\
     cluster fpc ClustGeo broom stringr jsonlite \
     ggmap shinycustomloader rdrop2\
     rgeos rgdal maptools sp spdep raster sf
     
## install Github packages
RUN installGithub.r ijlyttle/bsplus

## clean up
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds


EXPOSE 8080

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

## assume shiny app is in build folder /shiny
COPY ./newerhoods/ /srv/shiny-server/shiny/

#RUN chmod u+x entrypoint.sh
#ENTRYPOINT /app/entrypoint.sh


