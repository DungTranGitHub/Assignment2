library(tidyr)
library(dplyr)
library(cluster)
library(ggplot2)
library(ggmap)
library(viridis)
library(tidyverse)

#normalize data using z-transformation
ztransform <- function (data){
  (data - mean(data)) / sd(data)
}

#determine number of clusters
wssplot <- function(data, nc=15, seed=1234) {
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

latlong <- data[, colnames(data) %in% c(#"premisetype", 
                                        #"occurrenceyear", 
                                        #"occurrencemonth", 
                                        #"occurrencedayofweek", 
                                        #"occurrencehour", 
                                        #"MCI", 
                                        "Lat", 
                                        "Long")]


#formatting
latlong$occurrencedayofweek <- factor(as.integer(latlong$occurrencedayofweek))
latlong$occurrencemonth <- match(latlong$occurrencemonth, month.name)

#determine number of clusters
#we can see there's an elbow around 7 clusters
wssplot(latlong, nc=50)

# k-means 
k.means.fit <- kmeans(latlong, 34)
k.means.fit
str(k.means.fit)

torclus <- as.data.frame(k.means.fit$centers)
torclus$size <- k.means.fit$size

tormap <- get_map(location =c(left=-79.8129, bottom=43.4544, right=-78.9011, top=43.9132))

latlongclus <- latlong
latlongclus$cluster <- as.factor(k.means.fit$cluster)

ggmap(tormap) +
  geom_point( data= latlongclus, aes(x=Long[], y=Lat[], color= as.factor(cluster))) +
  theme_void() + coord_map() 

ggmap(tormap) +
  geom_point( data= torclus, aes(x=Long[], y=Lat[], size=size, color=size)) +
  theme_void() + coord_map() 

#i don't know how maps work, sorry

ggmap(tormap) +
  geom_point( data= torclus, aes(x=Long[], y=Lat[])) +
  theme_void() + coord_map() 

# tried to make a plot like this: https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2/ 
# but i couldn't...


#i also can't get hierarchical to work
d <- dist(latlong, method = "euclidean")
H.fit <- hclust(d, method="ward.D2")

#plot dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the clusters
rect.hclust(H.fit, k=4, border="red") 

# if we want to look at other numbers of clusters
counts <- sapply(2:6, function(ncl)table(cutree(H.fit, ncl)))
names(counts) <- 2:6
counts