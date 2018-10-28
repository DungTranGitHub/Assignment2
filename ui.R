#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Toronto Crimes", tabName = "toronto", icon = icon("th")),
    menuItem("Map And Cluster", icon = icon("dashboard"), tabName = "map", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "toronto",
            ## Section 1
            fluidRow(
              box(title = "Toronto Crime Overview", width = 12, solidHeader = TRUE, status = "primary",
                  box(
                    title = "Toronto By MCI", width = 6, height = 550, status = "primary",
                    plotOutput("torontoAll")
                  ),
                  
                  box(
                    title = "Toronto by Time Frame", width = 6, height = 550, status = "warning",
                    selectInput("byTimeOption", 
                                strong("Observe by:"), 
                                choices = list("Year","Month", "Day of Week", "Hour"),
                                selected = "Year"),
                    plotOutput("torontoByTimeOption")
                  ),
                  
                  box(
                    title = "Toronto Location Heatmap",  width = 12, height = 450, solidHeader = TRUE, status = "primary",
                    plotOutput("torontoHeatmap")
                  )
              )
            ),
            
            ## Section 2
            fluidRow(
              box(title = "Dangerous Zones v.s Safe Zones", width = 12, solidHeader = TRUE, status = "primary",
                  box(
                    width = 2, height = 500,
                    sliderInput("topN", "Top n:", 1, 9, 5),
                    selectInput("crimeTypeForTopN", 
                                strong("Type of Crime"), 
                                c("All", "Assault","Break and Enter", "Robbery", "Theft Over", "Auto Theft" )),
                    selectInput("byArea", 
                                strong("By"), 
                                c("Division", "Neighbourhood"))
                    
                  ),
                  box(
                    width = 5,height = 500,
                    title = "Dangerous Zone", background = "red",
                    plotOutput("topDangerous")
                  ),
                  box(
                    width = 5,height = 500,
                    title = "Safe Zone", background = "green",
                    plotOutput("topSafe")
                  )
              )
            ),
            
            ## Section 3
            fluidRow(
              ## Row 1
              box(title = "Knowing your neighbourhood", width = 12, solidHeader = TRUE, status = "primary",
                  box(width = 4,
                      selectInput("neighbourhood",
                                  "Neighbourhood to explore:",
                                  choices = NULL, selectize = FALSE)
                  ),
                  box(width = 4,
                      selectInput("typeOfCrime", "Type of Crime",
                                  choices = c("All", "Assault","Break and Enter", "Robbery", "Theft Over", "Auto Theft" )
                      )
                  ),
                  box(width = 4,
                      selectInput("occurredYear", "Occurrence Year",
                                  choices = list("All", 2017, 2016, 2015, 2014),
                                  selected = "All"
                      )
                  ),
                  box(width = 6,height = 450,title = "Crimes By Hour",
                      plotOutput("crimeTime")
                  ),
                  box(width = 6,height = 450,title = "Crimes By Weekday",
                      plotOutput("crimeDayOfWeek")
                  ),
                  
                  box(width = 6,height = 500,title = "Crimes By Month",
                      plotOutput("crimeMonth")
                  ),
                  box(width = 6,height = 500,
                      DT:: dataTableOutput("neighbourhoodTable")
                  )
              )
            )
    ),
    
    tabItem(tabName = 'map',
            h2("Your map is here")
    )
  )
)


# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(disable = TRUE),
  sidebar,
  body
)




