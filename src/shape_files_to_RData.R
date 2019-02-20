### saving shapefiles as RData for faster loading
census_tracts <- readOGR("../data/shapefiles/2010 Census Tracts/geo_export_4d4ca7d0-0c46-467e-8dee-99c93361f914.shp",stringsAsFactors = FALSE)
save(census_tracts,file = "../data/census_tracts.RData")

pumas <- readOGR("../data/shapefiles/Public Use Microdata Areas (PUMA)/geo_export_112df737-99d9-4599-8357-4c0b1e37faeb.shp")
save(pumas,file="../data/pumas.RData")

ntas <- readOGR("../data/shapefiles/nynta_18d/nynta.shp")
ntas <- spTransform(ntas,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
save(ntas,file="../data/ntas.RData")

cds <- readOGR("../data/shapefiles/nycd_18d/nycd.shp")
cds <- spTransform(cds,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
save(cds,file="../data/cds.RData")

precincts <- readOGR("../data/shapefiles/Police Precincts/geo_export_e7d15636-2d89-486a-bd6d-41a7dfa67b3d.shp")
precincts <- spTransform(precincts,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
save(precincts,file="../data/precincts.RData")

school_dists <- readOGR("../data/shapefiles/School Districts/geo_export_af10107a-e4ca-45b9-a03e-0717947ea03b.shp")
school_dists <- spTransform(school_dists,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
save(school_dists,file="../data/school_dists.RData")

