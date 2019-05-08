require(ggmap)
require(broom)
require(ggplot2)
require(gganimate)
require(transformr)
require(magick)
require(purrr)
require(stringr)

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

K <- 100
ct <- census_tracts

map <- get_stamenmap(bbox = c(left=-74.25559, bottom=40.49612,right=-73.70001,top=40.91553),
                     maptype = "toner-lite", source = "stamen", zoom = 11)

for(i in c(1:100)){
  ct <- census_tracts
  clusters <- data.frame(cl=cutree(tree,i))
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
                                 colour = "white", alpha = 0.6) + theme(legend.position="none") 
  ggsave(filename = paste0("./fig_output/nyc/nyc_",str_pad(i,3,side="left",pad="0"),".png"),
         width = 8,height=8,dpi = 150)
}

list.files(path = "./fig_output/nyc/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=4) %>% # animates, can opt for number of loops
  image_write("nyc_clustering.gif")


map <- get_stamenmap(bbox = c(left=-74.25559, bottom=40.49612,right=-73.70001,top=40.91553),
               maptype = "toner-lite", source = "stamen", zoom = 11)

polyplot <- function(data){
  p <- ggmap(map) + geom_polygon(data = fortify(data),
                            aes(long, lat, group = group, fill=id),
                            colour = "white", alpha = 0.6) + theme(legend.position="none") 
  
}

# p <- ggmap(map) + geom_polygon(data = fortify(tidy_tracts),
#                                aes(long, lat, group = group, fill=id),
#                                colour = "white", alpha = 0.6) + theme(legend.position="none") 


animation <- tween_polygon(tidy_tracts[tidy_tracts$iter == 1,],tidy_tracts[tidy_tracts$iter == 2,],
                           'cubic-in-out',10)

ani <- lapply(split(animation, animation$.frame),polyplot)
p <- p + transition_states(iter) 
p



# census_tracts@bbox
