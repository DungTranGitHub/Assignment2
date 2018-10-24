#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
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
library(ggplot2)
library(viridis)
library(tidyr)
library(cluster)
library(ggmap)
library(maps)

### data initial loading
model_dir = "models"
data_dir = "data"

### load data
data_initial=read.csv(paste(data_dir,"MCI_2014_to_2017.csv",sep="/"), header = TRUE, sep = ",")

toronto <- subset(data_initial, !duplicated(data_initial$event_unique_id))
toronto <- subset(toronto, !is.na(toronto$occurrenceyear))
toronto <- subset(toronto, !is.na(toronto$offence))
neighbourhoods  <-  unique(toronto$Neighbourhood)


shinyServer(function(input, output,session) {
  
  updateSelectInput(session, "neighbourhood", choices = neighbourhoods )
  
  filterData = reactiveValues(neighbourhoodData = toronto)
  
  observe({
    change = c(input$neighbourhood, input$typeOfCrime, input$occurredYear)
    if (!is.null(change)){
      changedData <- subset(toronto, (toronto$Neighbourhood == input$neighbourhood))
      if (input$occurredYear != "All"){
        changedData <- subset(changedData, changedData$occurrenceyear == input$occurredYear )
      }
      
      if (input$typeOfCrime != "All"){
        changedData <- subset(changedData, changedData$MCI == input$typeOfCrime)
      }
      filterData$neighbourhoodData <- changedData
    }
  })

  output$crimeTime <- renderPlot({
    
    if (input$typeOfCrime != "All"){
      hour_crime_group <- group_by(filterData$neighbourhoodData, occurrencehour)
      hour_crime <- dplyr::summarise(hour_crime_group, n=n())
      ggplot(aes(x=occurrencehour, y=n), data =hour_crime) + 
        geom_line(size=1.5, color = "orange") + 
        ggtitle('Crime by Hour') +
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
        ggtitle('Crime by Hour') +
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
          ggtitle("Crimes by Day of Week") +
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
        ggtitle("Crimes by Day of Week") +
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
          ggtitle('Crimes by Month') +
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
          ggtitle('Crimes by Month') +
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
