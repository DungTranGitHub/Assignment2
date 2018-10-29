#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(dplyr)
library(plyr)
library(ggpubr)
library(tidyverse)
library(caret)
library(ROCR)
library(pROC)
library(rlist)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(DMwR)
library(ggthemes)
library(glmnet)
library(e1071)
library(viridis)
library(tidyr)
library(cluster)
library(ggmap)
library(maps)
library(reshape2)
library(scales)
library(lubridate)
library(maptools)
library(RColorBrewer)
library(rgl)

### data initial loading
model_dir = "models"
data_dir = "data"
map_dir = "map"
saved_maps = list.files(map_dir)

### load data
data_initial=read.csv(paste(data_dir,"MCI_2014_to_2017.csv",sep="/"), header = TRUE, sep = ",")
### load map
for(file in saved_maps) {
  load(paste(map_dir,file,sep="/"))
}
### load neighbourhood
shpfile <- paste(data_dir,"NEIGHBORHOODS_WGS84_2.shp",sep="/")
sh <- readShapePoly(shpfile)
sh@data$AREA_S_CD <- as.integer(sh@data$AREA_S_CD)

### declare local variables
toronto <- subset(data_initial, !duplicated(data_initial$event_unique_id))
toronto <- subset(toronto, !is.na(toronto$occurrenceyear))
toronto <- subset(toronto, !is.na(toronto$offence))
toronto <- subset(toronto, !(is.na(toronto$occurrencedayofweek) | toronto$occurrencedayofweek == ""))
neighbourhoods  <-  unique(toronto$Neighbourhood)

### processing data for clustering 
data <- subset(data_initial, !duplicated(data_initial$event_unique_id))

#remove columns that aren't useful/duplicates
#duplicates of other columns, UCR codes - not used in this case, ID number - not needed
data <- data[, !colnames(data) %in% c("X","Y","Index_","event_unique_id","ucr_code","ucr_ext","FID")]
# Keep "Hood ID" because we need this when we plot clustering results on a map

#formatting dates - remove garbage time values at the end
data$occurrencedate <- gsub("(.*)T.*", "\\1", data$occurrencedate)
data$reporteddate <- gsub("(.*)T.*", "\\1", data$reporteddate)
data$occurrencetime = ymd_h(paste(data$occurrencedate,data$occurrencehour,sep = " "), tz="America/New_York")
data$reportedtime = ymd_h(paste(data$reporteddate,data$reportedhour,sep = " "), tz="America/New_York")
data$occurrencedate = ymd(data$occurrencedate)
data$reporteddate = ymd(data$reporteddate)

#removing whitespace from day of week
data$occurrencedayofweek <- as.factor(trimws(data$occurrencedayofweek, "b"))
data$reporteddayofweek <- as.factor(trimws(data$reporteddayofweek, "b"))

#missing data
#colSums(is.na(data))
NAdata <- unique (unlist (lapply (data, function (x) which (is.na (x)))))

#imputing occurence dates from occurence date field
data$occurrenceyear[NAdata] <- year(data$occurrencedate[NAdata])
data$occurrencemonth[NAdata] <- month(data$occurrencedate[NAdata], label = TRUE, abbr = FALSE)
data$occurrenceday[NAdata] <- day(data$occurrencedate[NAdata])
data$occurrencedayofweek[NAdata] <- wday(data$occurrencedate[NAdata], label = TRUE, abbr = FALSE)
data$occurrencedayofyear[NAdata] <- yday(data$occurrencedate[NAdata])

#replace space in string
data$offence <- gsub("\\s", "_", data$offence)
data$MCI <- gsub("\\s", "_", data$MCI)

#change things to factors
for(col in c("offence","MCI","Division","Hood_ID")) {
  data[,col] = as.factor(data[,col])
}

#change things to ordered factors, useful for daisy() later
for(col in c("reportedyear","reportedmonth","reportedday","reporteddayofyear","reporteddayofweek",
             "reportedhour","occurrenceyear","occurrencemonth","occurrenceday","occurrencedayofyear",
             "occurrencedayofweek","occurrencehour")) {
  data[,col] = ordered(data[,col])
}

#drop unused factor levels
for(col in names(data)) {
  if(is.factor(data[,col])) {
    data[,col] = droplevels(data[,col])
  }
}
### (3) Data outliers
data$occurrencedate <- ymd(gsub("(.*)T.*", "\\1", data$occurrencedate))
data$reporteddate <- ymd(gsub("(.*)T.*", "\\1", data$reporteddate))
data[which(data$occurrencedate < as.POSIXct("1970-01-01")),]

### Hood group for neighbourhood clustering
bygroup <- group_by(data, MCI, Hood_ID)
groups <- dplyr::summarise(bygroup, n=n())
groups <- groups[c("Hood_ID", "MCI", "n")]
hood <- as.data.frame(spread(groups, key=MCI, value=n))
hood_id = as.integer(hood[,"Hood_ID"])
hood = hood[,-1]
for(col in names(hood)) {
  hood[,col] = (hood[,col] - mean(hood[,col])) / sd(hood[,col])
}
# Hierarchichal Clustering
d <- dist(hood, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward.D2")

### Long Lat Clustering
latlong <- data[, colnames(data) %in% c("Lat", "Long")]
k.means.division <- kmeans(latlong, 34)

torclus <- as.data.frame(k.means.division$centers)
torclus$size <- k.means.division$size
latlongclus <- latlong
latlongclus$cluster <- as.factor(k.means.division$cluster)

## Hotspot Clustering
data2 <- data
data2$cluster <- k.means.division$cluster
bygroup <- group_by(data2, MCI, cluster)
groups <- dplyr::summarise(bygroup, n=n())
groups <- groups[c("cluster", "MCI", "n")]
hotspot <- as.data.frame(spread(groups, key=MCI, value=n))
hotspot <- hotspot[, -1]
for(col in names(hotspot)) {
  hotspot[,col] = (hotspot[,col] - mean(hotspot[,col])) / sd(hotspot[,col])
}


#### Shiny app
shinyServer(function(input, output,session) {
  
  updateSelectInput(session, "neighbourhood", choices = neighbourhoods )
  filterData = reactiveValues(neighbourhoodData = toronto)
  topNDataFilter = reactiveValues(topNGroup = NULL)
  neighbourhoodKMean = reactiveValues(kMeanClusters = NULL)
  hotspotKMean = reactiveValues(hotspotClusters = NULL)
  observe({
    neighbourhoodChange = c(input$neighbourhood, input$typeOfCrime, input$occurredYear)
    if (!is.null(neighbourhoodChange)){
      changedData <- subset(toronto, (toronto$Neighbourhood == input$neighbourhood))
      if (input$occurredYear != "All"){
        changedData <- subset(changedData, changedData$occurrenceyear == input$occurredYear )
      }
      
      if (input$typeOfCrime != "All"){
        changedData <- subset(changedData, changedData$MCI == input$typeOfCrime)
      }
      filterData$neighbourhoodData <- changedData
    }
    topNChange = c(input$crimeTypeForTopN, input$byArea)
    if(!is.null(topNChange)){

      if (input$crimeTypeForTopN != "All"){
        topNData <- subset(toronto, toronto$MCI == input$crimeTypeForTopN)
      } else {
        topNData <- toronto
      }
      if(input$byArea == "Division"){
        location_group <- group_by(topNData, Division)
      } else {
        location_group <- group_by(topNData, Neighbourhood)
      }
      crime_by_location <- dplyr::summarise(location_group, n=n())
      topNDataFilter$topNGroup <- crime_by_location
    }
    
    if(!is.null(input$clusterNo)){
      k.means.fit <- kmeans(hood, input$clusterNo)
      neighbourhoodKMean$kMeanClusters <- k.means.fit$cluster
    }
    
    if(!is.null(input$hotspotClusterNo)){
      k.means.fit <- kmeans(hotspot, input$hotspotClusterNo)
      hotspotKMean$hotspotClusters <- k.means.fit$cluster
    }
    
  })
  
  ##### Toronto Overview
  output$torontoAll <- renderPlot({
    crime_group <- group_by(toronto, MCI)
    crime_by_type <- dplyr::summarise(crime_group, Occurrences=n())
    crime_by_type <- crime_by_type[order(crime_by_type$Occurrences, decreasing = TRUE),]
    ggplot(aes(x = reorder(MCI, Occurrences) , y = Occurrences), data = crime_by_type) +
      geom_bar(stat = 'identity', position = position_dodge(), width = 0.5) +
      geom_text(aes(label = Occurrences), stat = 'identity', data = crime_by_type, hjust = 1.1, size = 3.5, color = "white") +
      coord_flip() +
      xlab('Major Crime Indicators') +
      ylab('Number of Occurrences') +
      theme_minimal() +
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
  })
  
  ### Toronto by Time Frame
  output$torontoByTimeOption <- renderPlot({
    
    if(input$byTimeOption == "Hour"){
      option_group <- group_by(toronto, occurrencehour, MCI)
      option_crime <- dplyr::summarise(option_group, n=n())
      ggplot(aes(x=occurrencehour, y=n, color=MCI), data =option_crime) + 
        geom_line(size=1.5) + 
        ylab('Number of Occurrences') +
        xlab('Hour(24-hour clock)') +
        theme_grey() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"))
      
    }else if (input$byTimeOption == "Day of Week"){
      day_group <- group_by(toronto, occurrencedayofweek, MCI)
      day_count <- dplyr::summarise(day_group,Total = n())
      ggplot(day_count, aes(occurrencedayofweek, MCI, fill = Total)) +
        geom_tile(size = 1, color = "white") +
        scale_fill_viridis()  +
        geom_text(aes(label=Total), color='white') +
        xlab('Day of Week') +
        theme(plot.title = element_text(size = 16), 
              axis.title = element_text(size = 12, face = "bold"))
      
    }else if (input$byTimeOption == "Month"){
      month_group <- group_by(toronto, occurrencemonth, MCI) 
      month_count <- dplyr::summarise(month_group, n=n())
      month_count$occurrencemonth <- ordered(month_count$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
      ggplot(aes(x = occurrencemonth, y = n, fill = MCI), data = month_count) +
        geom_bar(stat = 'identity',position = "stack", width = 0.6) +
        coord_flip() +
        xlab('Month') +
        ylab('Number of Occurrences') +
        theme_bw() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"))
      
    }else if (input$byTimeOption == "Year"){
      year_data <- subset(toronto,!(toronto$occurrenceyear < 2014))
      option_group <- group_by(year_data, occurrenceyear, MCI)
      option_crime <- dplyr::summarise(option_group, n=n())
      ggplot(aes(x=occurrenceyear, y=n, color=MCI), data =option_crime) + 
        geom_line(size=1.5) + 
        ylab('Number of Occurrences') +
        xlab('Year)') +
        theme_grey() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"))
    }
  })
  
  ### Heatmap
  output$torontoHeatmap <- renderPlot({
    base_size <- 9
    heat_group <- group_by(toronto, Neighbourhood, offence)
    heat_count <- dplyr::summarise(heat_group,Total = n())
    heat_count$Neighbourhood <- with(heat_count,reorder(Neighbourhood,Total))
    heat_count.m <- melt(heat_count)
    heat_count.m <- ddply(heat_count.m, .(variable), transform,rescale = rescale(value))
    ggplot(heat_count.m, aes(Neighbourhood, offence)) + 
        geom_tile(aes(fill = rescale),colour = "white") + 
        scale_fill_gradient(low = "lightblue",high = "darkblue") +
        theme_grey(base_size = base_size) + 
        labs(x = "", y = "") + 
        scale_x_discrete(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        theme_minimal() +
        theme(legend.position = "none",axis.ticks = element_blank(), axis.text.x = element_text(size = base_size *0.8, angle = 330, hjust = 0, colour = "grey50"))
    
  })
  
  ### Top n
  output$topDangerous <- renderPlot({
    crime_dangerous <- topNDataFilter$topNGroup[order(topNDataFilter$topNGroup$n, decreasing = TRUE), ]
    crime_dangerous_top <- head(crime_dangerous, input$topN)
    
    if (input$byArea == "Division"){
        ggplot(aes(x = reorder(Division, n), y = n, fill = reorder(Division, n)), data = crime_dangerous_top) +
          geom_bar(stat = 'identity',  width = 0.6) +
          coord_flip() +
          xlab('Zone') +
          ylab('Number of Occurrences') +
          scale_fill_brewer(palette = "Reds") +
          theme(plot.title = element_text(size = 16),
                axis.title = element_text(size = 12, face = "bold"),
                legend.position="none")
    } else {
        ggplot(aes(x = reorder(Neighbourhood, n), y = n, fill = reorder(Neighbourhood, n)), data = crime_dangerous_top) +
          geom_bar(stat = 'identity', width = 0.6) +
          coord_flip() +
          xlab('Zone') +
          ylab('Number of Occurrences') +
          scale_fill_brewer(palette = "Reds") +
          theme(plot.title = element_text(size = 16),
                axis.title = element_text(size = 12, face = "bold"),
                legend.position="none")
        
    }
    
  })
  
  output$topSafe <- renderPlot({
    crime_safe <- topNDataFilter$topNGroup[order(topNDataFilter$topNGroup$n, decreasing = FALSE), ]
    crime_safe_top <- head(crime_safe, input$topN)
    
    if (input$byArea == "Division"){
      ggplot(aes(x = reorder(Division, -n), y = n, fill = reorder(Division, -n)), data = crime_safe_top) +
        geom_bar(stat = 'identity',  width = 0.6) +
        coord_flip() +
        xlab('Zone') +
        ylab('Number of Occurrences') +
        scale_fill_brewer(palette = "Greens") +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"),
              legend.position="none")
    } else {
      ggplot(aes(x = reorder(Neighbourhood, -n), y = n, fill = reorder(Neighbourhood, -n)), data = crime_safe_top) +
        geom_bar(stat = 'identity', width = 0.6) +
        coord_flip() +
        xlab('Zone') +
        ylab('Number of Occurrences') +
        scale_fill_brewer(palette = "Greens") +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"),
              legend.position="none")
      
    }
    
  })

  
  
  ### Neighbourhood analysis
  
  output$crimeTime <- renderPlot({
    
    if (input$typeOfCrime != "All"){
      hour_crime_group <- group_by(filterData$neighbourhoodData, occurrencehour)
      hour_crime <- dplyr::summarise(hour_crime_group, n=n())
      ggplot(aes(x=occurrencehour, y=n), data =hour_crime) + 
        geom_line(size=1.5, color = "orange") + 
        #ggtitle('Crime by Hour') +
        ylab('Number of Occurrences') +
        xlab('Hour(24-hour clock)') +
        theme_bw() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"))
    } else {
      hour_crime_group <- group_by(filterData$neighbourhoodData, occurrencehour, MCI)
      hour_crime <- dplyr::summarise(hour_crime_group, n=n())
      ggplot(aes(x=occurrencehour, y=n, color=MCI), data =hour_crime) + 
        geom_line(size=1.5) + 
        #ggtitle('Crime by Hour') +
        ylab('Number of Occurrences') +
        xlab('Hour(24-hour clock)') +
        theme_grey() +
        theme(plot.title = element_text(size = 16),
              axis.title = element_text(size = 12, face = "bold"))
    }
  })
  
  output$crimeDayOfWeek <- renderPlot({
    if (input$typeOfCrime != "All"){
      day_group <- group_by(filterData$neighbourhoodData, occurrencedayofweek, offence)
      day_count <- dplyr::summarise(day_group,Total = n())
      ggplot(day_count, aes(occurrencedayofweek, offence, fill = Total)) +
          geom_tile(size = 1, color = "white") +
          scale_fill_viridis()  +
          geom_text(aes(label=Total), color='white') +
          #ggtitle("Crimes by Day of Week") +
          xlab('Day of Week') +
          theme(plot.title = element_text(size = 16), 
                axis.title = element_text(size = 12, face = "bold"))
    } else {
      day_group <- group_by(filterData$neighbourhoodData, occurrencedayofweek, MCI)
      day_count <- dplyr::summarise(day_group,Total = n())
      ggplot(day_count, aes(occurrencedayofweek, MCI, fill = Total)) +
        geom_tile(size = 1, color = "white") +
        scale_fill_viridis()  +
        geom_text(aes(label=Total), color='white') +
        xlab('Day of Week') +
        theme(plot.title = element_text(size = 16), 
              axis.title = element_text(size = 12, face = "bold"))
    }
    
  })
  
  output$crimeMonth <- renderPlot({
  
    if (input$typeOfCrime != "All"){
        month_group <- group_by(filterData$neighbourhoodData, occurrencemonth) 
        month_count <- dplyr::summarise(month_group, n=n())
        month_count$occurrencemonth <- ordered(month_count$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
        ggplot(aes(x = occurrencemonth, y = n), data = month_count) +
          geom_bar(stat = 'identity',position = "dodge", width = 0.6) +
          coord_flip() +
          xlab('Month') +
          ylab('Number of Occurrences') +
          #ggtitle('Crimes by Month') +
          scale_fill_brewer(
            palette = "Blues") +
          theme(plot.title = element_text(size = 16),
                axis.title = element_text(size = 12, face = "bold"))
    } else {
        month_group <- group_by(filterData$neighbourhoodData, occurrencemonth, MCI) 
        month_count <- dplyr::summarise(month_group, n=n())
        month_count$occurrencemonth <- ordered(month_count$occurrencemonth, levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))
        ggplot(aes(x = occurrencemonth, y = n, fill = MCI), data = month_count) +
          geom_bar(stat = 'identity',position = "stack", width = 0.6) +
          coord_flip() +
          xlab('Month') +
          ylab('Number of Occurrences') +
          #ggtitle('Crimes by Month') +
          theme_bw() +
          theme(plot.title = element_text(size = 16),
                axis.title = element_text(size = 12, face = "bold"))
        
    }
  })
  

  output$neighbourhoodTable <- DT:: renderDataTable({
    group <-  group_by(filterData$neighbourhoodData, MCI, offence)
    tableData <- dplyr::summarise(group, Total=n())
  })
  
  
  ### Map and Clustering
  
  ### Strategy I - Clustering By Neighbourhood
  output$manualMap <- renderPlot({
    total_offence_cnt_table = data %>% group_by(Hood_ID) %>% dplyr::summarise(offence_cnt = n())
    hood_total_offence_cnt_table = merge(total_offence_cnt_table,sh@data,by.x='Hood_ID',by.y='AREA_S_CD')
    points_offense_cnt <- fortify(sh, region = 'AREA_S_CD')
    points_offense_cnt <- merge(points_offense_cnt, hood_total_offence_cnt_table, by.x='id', by.y='Hood_ID', all.x=TRUE)
    torontoMap + geom_polygon(aes(x=long,y=lat, group=group, fill=offence_cnt), data=points_offense_cnt, color='black') +
      scale_fill_distiller(palette='Spectral') + scale_alpha(range=c(0.5,0.5))
  })
  

  output$kMeanElbow <- renderPlot({
    
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
    
  })
  
  output$`2DkMeanCluster` <- renderPlot({
    clusplot(hood, neighbourhoodKMean$kMeanClusters,
             color=TRUE, shade=TRUE,
             labels=2, lines=0)
  })
  
  output$`3DkMeanCluster` <- renderRglwidget({
    # k-means
    pc <-princomp(hood, cor=TRUE, scores=TRUE)
    rgl.open(useNULL = T)
    rgl.bg(color = "white" )
    rgl.spheres(pc$scores[,1:3], r = 0.2, col=neighbourhoodKMean$kMeanClusters)
    rgl.bbox(color=c("#333377","black"), emission="#333377",
              specular="#3333FF", shininess=5, alpha=0.8, xlen=5, ylen=5, zlen=2, marklen=15.9) 
    rglwidget()
  })
  
  output$kMeanMap <- renderPlot({
    cluster_ids <- neighbourhoodKMean$kMeanClusters
    hood_ids_and_cluster_ids <- data.frame(cbind(hood_id,cluster_ids))
    hood_ids_and_cluster_ids$cluster_ids = as.factor(hood_ids_and_cluster_ids$cluster_ids)
    hood_name_and_cluster_ids = merge(hood_ids_and_cluster_ids,sh@data,by.x='hood_id',by.y='AREA_S_CD')
    points_clustering <- fortify(sh, region = 'AREA_S_CD')
    points_clustering <- merge(points_clustering, hood_name_and_cluster_ids, by.x='id', by.y='hood_id', all.x=TRUE)
    torontoMap + geom_polygon(aes(x=long,y=lat, group=group, fill=cluster_ids), data=points_clustering, color='black') +
    scale_fill_brewer(palette = "Set2")
  })
  
  output$clusterDiagram <- renderPlot({
    plot(H.fit)
    rect.hclust(H.fit, k=input$clusterNo, border="red") 
  })
  
  output$`2DHierarchicalCluster` <- renderPlot({
    clusplot(hood, cutree(H.fit, k=input$clusterNo) ,
             color=TRUE, shade=TRUE,
             labels=2, lines=0)
  })
  
  output$`3DHierarchicalCluster` <- renderRglwidget({
    pc <-princomp(hood, cor=TRUE, scores=TRUE)
    rgl.open(useNULL = T)
    rgl.bg(color = "white" )
    rgl.spheres(pc$scores[,1:3], r = 0.2, col=cutree(H.fit, k=input$clusterNo) )
    rgl.bbox(color=c("#333377","black"), emission="#333377",
             specular="#3333FF", shininess=5, alpha=0.8, xlen=5, ylen=5, zlen=2, marklen=15.9) 
    rglwidget()
  })
  
  output$hierarchicalMap <- renderPlot({
    cluster_ids <- cutree(H.fit, k=input$clusterNo)
    hood_ids_and_cluster_ids <- data.frame(cbind(hood_id,cluster_ids))
    hood_ids_and_cluster_ids$cluster_ids = as.factor(hood_ids_and_cluster_ids$cluster_ids)
    hood_name_and_cluster_ids = merge(hood_ids_and_cluster_ids,sh@data,by.x='hood_id',by.y='AREA_S_CD')
    points_clustering <- fortify(sh, region = 'AREA_S_CD')
    points_clustering <- merge(points_clustering, hood_name_and_cluster_ids, by.x='id', by.y='hood_id', all.x=TRUE)
    torontoMap + geom_polygon(aes(x=long,y=lat, group=group, fill=cluster_ids), data=points_clustering, color='black') +
      scale_fill_brewer(palette = "Set2")
  })
  
  output$divisionMap <- renderPlot({
    torontoMap +
      geom_point( data= latlongclus, aes(x=Long[], y=Lat[], color= as.factor(cluster))) +
      theme_void() + coord_map() 
  })
  
  output$divisionCentroid <- renderPlot({
    torontoMap +
      geom_point( data= torclus, aes(x=Long[], y=Lat[], size=size)) +
      theme_void() + coord_map()
  })
  
  output$hotspotElbow <- renderPlot({
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
    wssplot(hotspot, nc=15)
  })
  
  output$`2DHotspotCluster` <- renderPlot({
    clusplot(hotspot, hotspotKMean$hotspotClusters,
             color=TRUE, shade=TRUE,
             labels=2, lines=0)
  })
  
  output$`3DHotspotCluster` <- renderRglwidget ({
    pc <-princomp(hotspot, cor=TRUE, scores=TRUE)
    rgl.open(useNULL = T)
    rgl.bg(color = "white" )
    rgl.spheres(pc$scores[,1:3], r = 0.2, col=hotspotKMean$hotspotClusters)
    rgl.bbox(color=c("#333377","black"), emission="#333377",
             specular="#3333FF", shininess=5, alpha=0.8, xlen=5, ylen=5, zlen=2, marklen=15.9) 
    rglwidget()
  })
  
  output$hotSpotMap <- renderPlot({
    clusters <- as.factor(hotspotKMean$hotspotClusters)
    torontoMap +
      geom_point( data= torclus, aes(x=Long[], y=Lat[], color= clusters, size = 5 )) +
      theme_void() + coord_map()
  })
  

})
