### Clustering

library(cluster)
library(dbscan)
library(spdep)
library(rgdal)
library(fpc)
library(readxl)
library(dplyr)
library(ClustGeo)
library(RColorBrewer)
library(sp)

### Load feature set
census_tracts <- readOGR("data/shapefiles/2010 Census Tracts/geo_export_4d4ca7d0-0c46-467e-8dee-99c93361f914.shp")
load("./data/sales_features_2017.RData")

census_pop <- read_xlsx("./data/cleaned/2010census_population.xlsx",skip=7,
                        col_names=c("borough","county_code","borough_code",
                                    "2010_tract","pop_2000","pop_2010",
                                    "change","pct_change","acres","pop_per_acre"))

census_pop$boro_ct201 <- paste0(census_pop$borough_code,census_pop$`2010_tract`)

### Removing Tracts with low population
plot(density(census_pop$pop_2010))
abline(v=500,col=2)
abline(v=100,col=3)



## population based exclusion (population < 500) based on above density
tracts_to_exclude <- c(census_pop$boro_ct201[census_pop$pop_2010 <= 500],"1023802") ##Rosevelt Island
cols <- rep('blue',length(census_tracts$boro_ct201))
cols[census_tracts$boro_ct201 %in% tracts_to_exclude] <- 'green'
plot(census_tracts,col=cols)


reduced_tracts <- census_tracts[!(census_tracts$boro_ct201 %in% tracts_to_exclude),]

sales_features <- sales_features[!(sales_features$boro_ct201 %in% tracts_to_exclude),]
### geographical clustering
features_to_use <- !grepl("year|lon|lat|boro_ct201",colnames(sales_features))
features_to_use <- grepl("age",colnames(sales_features))

D0 <- dist(scale(sales_features[,features_to_use]))

list.nb <- poly2nb(reduced_tracts)
A <- nb2mat(list.nb,style = "B",zero.policy = TRUE)
diag(A) <- 1
D1 <- as.dist(1-A)

range.alpha <- seq(0,1,0.1)
cr <- choicealpha(D0,D1,range.alpha,55,graph=TRUE)
cr$Qnorm

K <- 55
colours <- distinctColorPalette(K)
tree <- hclustgeo(D0,D1,alpha=0.1)
clusters <- data.frame(cl=cutree(tree,K))
clusters$boro_ct201 <- sales_features$boro_ct201
clusters <- left_join(data.frame(boro_ct201=census_tracts$boro_ct201),clusters,by="boro_ct201")
clusters$cl[is.na(clusters$cl)] <- K+1
colours <- c(colours,"#A9A9A9A9") ## grey for excluded tracts
plot(census_tracts,col=colours[clusters$cl])




tree <- hclustgeo(D0)
cl <- c(2:60)
sil <- NULL
ch <- NULL
for(i in cl){
  pi <- cutree(tree,k=i)
  cl_stats <- cluster.stats(D0,clustering=pi)
  sil[i] <- cl_stats$avg.silwidth  
  ch[i] <- cl_stats$ch
}

plot(x=cl[2:48],y=sil[2:48],type='l')
plot(x=cl[2:48],y=ch[2:48],type='l')



# To Do:
# Predefined setting
### Reshaped Boroughs? (k=5)
### Redefined neighborhoods (k= size(PUMAs) = 55)
### Optimal #clusters (k= x)
