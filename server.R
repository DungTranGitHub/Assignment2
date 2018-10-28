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

### data initial loading
model_dir = "models"
data_dir = "data"

### load data
data_initial=read.csv(paste(data_dir,"MCI_2014_to_2017.csv",sep="/"), header = TRUE, sep = ",")

toronto <- subset(data_initial, !duplicated(data_initial$event_unique_id))
toronto <- subset(toronto, !is.na(toronto$occurrenceyear))
toronto <- subset(toronto, !is.na(toronto$offence))
toronto <- subset(toronto, !(is.na(toronto$occurrencedayofweek) | toronto$occurrencedayofweek == ""))
neighbourhoods  <-  unique(toronto$Neighbourhood)
sort(neighbourhoods)

shinyServer(function(input, output,session) {
  
  updateSelectInput(session, "neighbourhood", choices = neighbourhoods )
  filterData = reactiveValues(neighbourhoodData = toronto)
  topNDataFilter = reactiveValues(topNGroup = NULL)
  
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

})
