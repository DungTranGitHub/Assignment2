library(tidyr)
library(dplyr)
library(cluster)

#clustering crime by police division and neighbourhood

#neighbourhood first
#first, coerce the data into a table that can be clustered - we aren't interested in the occurence date at this point
#courtesy of Susan Li - https://datascienceplus.com/exploring-clustering-and-mapping-torontos-crimes/
bygroup <- group_by(data, MCI, Neighbourhood)
groups <- summarise(bygroup, n=n())
groups <- groups[c("Neighbourhood", "MCI", "n")]
hood <- spread(groups, key=MCI, value=n)
hood <- data.frame(hood[, -1])

#normalize data using z-transformation
ztransform <- function (data){
  (data - mean(data)) / sd(data)
}

for(col in names(hood)) {
  hood[,col] = ztransform(hood[,col])
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

#we can see there's an elbow around 3 clusters
wssplot(hood, nc=15)

# k-means
k.means.fit <- kmeans(hood, 3)
k.means.fit

#cluster 3 has lower than average crime, while cluster 2 has higher than average. cluster 1 sits in the middle for incidents
#cluster 3 is also the one with the most number of neighbourhoods which implies that the majority of toronto is very safe
#there are only 10 neighbourhoods in cluster 2, which means crime is concentrated in these small pockets of toronto

#plotting k-means
clusplot(hood, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#we can see there is some overlap between cluster 1 and 3, which means they could potentially be one cluster
#implies that they are more similar to each other than they are to cluster 2

#hierarchical
d <- dist(hood, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward.D2")

#plot dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=3, border="red") 

# if we want to look at other numbers of clusters
counts <- sapply(2:6, function(ncl) table(cutree(H.fit, ncl)))
names(counts) <- 2:6
counts

# we can see that 3 clusters is likely ideal because we don't want to split many clusters with small numbers of neighbourhoods

#now do by division
#coerce data
bygroup <- group_by(data, MCI, Division)
groups <- summarise(bygroup, n=n())
groups <- groups[c("Division", "MCI", "n")]
div <- spread(groups, key=MCI, value=n)
div <- div[, -1]

#normalize
div$Assault <- ztransform(div$Assault)
div$`Auto Theft` <- ztransform(div$`Auto Theft`)
div$`Break and Enter` <- ztransform(div$`Break and Enter`)
div$Robbery <- ztransform(div$Robbery)
div$`Theft Over` <- ztransform(div$`Theft Over`)

#determine number of clusters
#we can see there's an elbow around 3 clusters
wssplot(div, nc=15)

# k-means
k.means.fit <- kmeans(div, 3)
k.means.fit

#similar to neighbourhoods, two clusters have low crime incidents (1 and 2), while cluster 3 has higher crime incidents
# most districts are lower crime incident districts, while 9 specifically are higher
#can we map this and see if they correspond to neighbourhoods?
#do the toronto police have a dedicated district to each high crime neighbourhood?

#plotting k-means
clusplot(div, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#not much overlap! we can see cluster 2 is very tightly packed which shows that those districts are very similar to one another for crime
# cluster 2 has lower crime
# there is a larger spread for 1 and 3

#hierarchical
d <- dist(div, method = "euclidean")
H.fit <- hclust(d, method="ward.D2")

#plot dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=3, border="red") 

# if we want to look at other numbers of clusters
counts <- sapply(2:6, function(ncl)table(cutree(H.fit, ncl)))
names(counts) <- 2:6
counts
#we can still see that 3 clusters looks to be the best number of clusters, without splitting them too deeply

#clusteriung types of crime by occurence month/hour/date?

#repeat for month
bygroup <- group_by(data, MCI, occurrencemonth)
groups <- summarise(bygroup, n=n())
groups <- groups[c("occurrencemonth", "MCI", "n")]
mon <- spread(groups, key=MCI, value=n)
mon <- mon[, -1]

#normalize
mon$Assault <- ztransform(mon$Assault)
mon$`Auto Theft` <- ztransform(mon$`Auto Theft`)
mon$`Break and Enter` <- ztransform(mon$`Break and Enter`)
mon$Robbery <- ztransform(mon$Robbery)
mon$`Theft Over` <- ztransform(mon$`Theft Over`)

#determine number of clusters
#we can see there's an elbow around 2 clusters
wssplot(mon, nc=10)

# k-means 
k.means.fit <- kmeans(mon, 2)
k.means.fit

#7 months of the year (cluster 2) have higher crime incidents than the other 5 months of the year

#plotting k-means
clusplot(mon, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#not much overlap! means these months are quite different

#hierarchical
d <- dist(mon, method = "euclidean")
H.fit <- hclust(d, method="ward.D2")

#plot dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=2) # cut tree into 2 clusters
# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k=2, border="red") 

# if we want to look at other numbers of clusters
counts <- sapply(2:6, function(ncl)table(cutree(H.fit, ncl)))
names(counts) <- 2:6
counts
#2 clusters makes the most sense, since we start to get into clusters with 1 month when we split into 3