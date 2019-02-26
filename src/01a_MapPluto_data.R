## Loading and merging MapPluto Data
library(rgdal)
library(rgeos)
library(dplyr)

## Loading MapPluto Data by Borough

## Source URL: https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-pluto-mappluto.page

bk_pluto <- readOGR("./data/raw/shapefiles/bk_mappluto_18v1/BKMapPLUTO.dbf")
mn_pluto <- readOGR("./data/raw/shapefiles/mn_mappluto_18v1/MNMapPLUTO.dbf")
bx_pluto <- readOGR("./data/raw/shapefiles/bx_mappluto_18v1/BXMapPLUTO.dbf")
qn_pluto <- readOGR("./data/raw/shapefiles/qn_mappluto_18v1/QNMapPLUTO.dbf")
si_pluto <- readOGR("./data/raw/shapefiles/si_mappluto_18v1/SIMapPLUTO.dbf")

pluto_data <- list(mn_pluto,bk_pluto,bx_pluto,qn_pluto,si_pluto)


## Merging it into one. Extracting centroid of each from the polygon.
bbl_coords <- data.frame(matrix(NA,1,31))
lat_lons <- data.frame(matrix(NA,1,2))

colnames(lat_lons) <- c("x","y")

for(i in c(1:5)){
  tmp <- as.data.frame(pluto_data[[i]]@data[,c(1:13,17,35:48,72,74,84)])
  colnames(bbl_coords) <- colnames(tmp)
  bbl_coords <- rbind(bbl_coords,tmp)
  lat_lons <- rbind(lat_lons,as.data.frame(spTransform(gCentroid(pluto_data[[i]],byid=TRUE),CRS("+proj=longlat +datum=WGS84"))))
}

colnames(lat_lons) <- c("lon","lat")
bbl_coords <- cbind(bbl_coords,lat_lons)
bbl_coords <- bbl_coords[-1,]

bbl_coords$Block <- as.numeric(bbl_coords$Block)
bbl_coords <- bbl_coords %>% mutate(boro=case_when( Borough == "MN" ~ 1,
                                                    Borough == "BX" ~ 2,
                                                    Borough == "BK" ~ 3,
                                                    Borough == "QN" ~ 4,
                                                    Borough == "SI" ~ 5))


save(bbl_coords,file="./data/intermediary/bbl_coords.RData")
