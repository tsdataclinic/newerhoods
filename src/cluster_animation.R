require(ggmap)
require(broom)
require(ggplot2)
require(gganimate)
require(transformr)
require(magick)
require(purrr)
require(stringr)
require(dplyr)
require(ClustGeo)

load(file="newerhoods/clean_data/pre_compiled_data.RData")

features$boro <- as.numeric(substr(features$boro_ct201,1,1))
# features <- features[features$boro == 5,]
features_to_use <- grepl("med_price_1y|sd_price_1y",colnames(features))
feature_set <- unlist(strsplit("med_price_1y|sd_price_1y","\\|"))

### Clustering the data based on selected features
D0 <- dist(scale(features[,features_to_use]))
# si_tracts <- which(features$boro == 5)
# D2 <- as.dist(as.matrix(D1)[si_tracts,si_tracts])
set.seed(1729)
alpha=0.15
tree <- hclustgeo(D0,D1,alpha=0.15)
# plot(tree,labels=FALSE)

K <- 100
ct <- census_tracts

map <- get_stamenmap(bbox = c(left=-74.25559, bottom=40.49612,right=-73.70001,top=40.91553),
                     maptype = "toner-lite", source = "stamen", zoom = 11)

x <- c(1:10,seq(from=0,to=2100,by=75))
x[11] <- 30
x <- x[1:length(x)]
x[length(x)] <- 2096

for(i in c(1:length(x))){
  ct <- census_tracts
  clusters <- data.frame(cl=cutree(tree,x[i]))
  clusters$boro_ct201 <- features$boro_ct201
  clusters <- left_join(data.frame(boro_ct201=ct$boro_ct201,
                                   stringsAsFactors = FALSE),
                        clusters,by="boro_ct201")
  ct@data$cl <- clusters$cl
  ct <- ct[order(census_tracts$boro_ct201),]
  # ct <- ct[sample(c(1:dim(ct)[1]),dim(ct)[1],replace = FALSE),]
  tt <- tidy(ct,region = "cl")
  tt <- as.data.frame(tt, stringsAsFactors = FALSE)
  # tt$iter <- i
  p <- ggmap(map) + geom_polygon(data = fortify(tt),
                                 aes(long, lat, group = group, fill=as.factor(as.numeric(id))),
                                 colour = "white", alpha = 0.75) + 
    theme(legend.position="none") + ggtitle(label=paste0("Number of neighborhoods = ",x[i]))
  ggsave(filename = paste0("./fig_output/nyc2/nyc_",str_pad(i,4,side="left",pad="0"),".png"),
         width = 8,height=8,dpi = 150)
}

list.files(path = "./fig_output/nyc2/", pattern = "*.png", full.names = T) %>% 
  sort(decreasing = TRUE) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("nyc_clustering2.gif")



for(i in c(1:21)){
  alpha=0.05*(i-1)
  tree <- hclustgeo(D0,D1,alpha=alpha)
  ct <- census_tracts
  clusters <- data.frame(cl=cutree(tree,50))
  clusters$boro_ct201 <- features$boro_ct201
  clusters <- left_join(data.frame(boro_ct201=ct$boro_ct201,
                                   stringsAsFactors = FALSE),
                        clusters,by="boro_ct201")
  ct@data$cl <- clusters$cl
  ct <- ct[order(census_tracts$boro_ct201),]
  tt <- tidy(ct,region = "cl")
  tt <- as.data.frame(tt, stringsAsFactors = FALSE)
  # tt$iter <- i
  p <- ggmap(map) + geom_polygon(data = fortify(tt),
                                 aes(long, lat, group = group, fill=id),
                                 colour = "white", alpha = 0.6) + 
    theme(legend.position="none") + ggtitle(label=paste0("alpha= ",alpha,", k = 50"))
  ggsave(filename = paste0("./fig_output/nyc_alpha/nyc_",str_pad(i,3,side="left",pad="0"),".png"),
         width = 8,height=8,dpi = 150)
}

list.files(path = "./fig_output/nyc_alpha/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("nyc_alpha_clustering.gif")


ca <- choicealpha(D0,D1,range.alpha = seq(0,1,0.1),K=100)
