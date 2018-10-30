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
library(rgl)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Toronto Crimes", tabName = "toronto", icon = icon("th")),
    menuItem("Clustering By Neighbourhood", icon = icon("dashboard"), tabName = "neighbourhoodClustering", badgeColor = "green"),
    menuItem("Clustering By Long Lat", icon = icon("dashboard"), tabName = "longlatClustering", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "toronto",
            ## Section 1
            fluidRow(
              box(title = "Report", width = 12, solidHeader = TRUE, status = "warning",
                  strong("Please click ",
                         tags$a(href='reportForHtml.html',' here', target='blank'), " to view Toronto Crimes project's report.")
              ),
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
    
    tabItem(tabName = 'neighbourhoodClustering',
            fluidRow(
              box(title = "Strategy I - Clustering By Neighbourhood", width = 12, solidHeader = TRUE, status = "warning",
                  box(
                    title ="Number of Clusters", width = 3,
                    sliderInput("clusterNo", "Top n:", 3, 6, 3)
                  ),
                  box(title = "Toronto Criminal Map By Neighbourhoods", width = 12, solidHeader = TRUE, status = "primary",
                      box(
                        title = "Manual Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("manualMap")
                      ),
                      
                      box(
                        title = "kMean Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("kMeanMap")
                      ),
                      
                      box(
                        title = "Hierarchical Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("hierarchicalMap")
                      )
                  ),
                  box(
                    title = "kMean Clustering", width = 12, height = 600, solidHeader = TRUE, status = "primary",
                    box(
                      title = "Determine number of clusters", width = 4,height = 530, status = "warning",
                      plotOutput("kMeanElbow")
                    ),
                    box(
                      title = "2D kMean Clustering", width = 4,height = 530, status = "warning",
                      plotOutput("2DkMeanCluster")
                    ),
                    box(
                      title = "3D kMean Clustering", width = 4,height = 530, status = "warning",
                      rglwidgetOutput("3DkMeanCluster")
                    )
                  ),
                  box(
                    title = "Hierarchical Clustering", width = 12, height = 600, solidHeader = TRUE, status = "primary",
                    box(
                      title = "Hiearchical Cluster Diagram", width = 4,height = 530, status = "warning",
                      plotOutput("clusterDiagram")
                    ),
                    box(
                      title = "2D Hierarchical Clustering", width = 4,height = 530, status = "warning",
                      plotOutput("2DHierarchicalCluster")
                    ),
                    box(
                      title = "3D Hierarchichal Clustering", width = 4,height = 530, status = "warning",
                      rglwidgetOutput("3DHierarchicalCluster")
                    )
                  )
              )
            )
    ),
    tabItem(tabName = 'longlatClustering',
            fluidRow(
              box(title = "Strategy II - Clustering By Long Lat", width = 12, solidHeader = TRUE, status = "warning",
                  box(
                    title ="Number of Hotspot Clusters", width = 3,
                    sliderInput("hotspotClusterNo", "Top n:", 3, 6, 4)
                  ),
                  box(title = "Toronto Criminal Map By Long Lat", width = 12, solidHeader = TRUE, status = "primary",
                      box(
                        title = "Division Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("divisionMap")
                      ),
                      
                      box(
                        title = "Division Criminal Centroid Map", width = 4, height = 450, status = "warning",
                        plotOutput("divisionCentroid")
                      ),
                      
                      box(
                        title = "Hotspot Clustering Map", width = 4, height = 450, status = "warning",
                        plotOutput("hotSpotMap")
                      )
                      
                  ),
                  box(
                    title = "Hotspot Clustering", width = 12, height = 600, solidHeader = TRUE, status = "primary",
                    box(
                      title = "Determine number of clusters", width = 4,height = 530, status = "warning",
                      plotOutput("hotspotElbow")
                    ),
                    box(
                      title = "2D kMean Clustering", width = 4,height = 530, status = "warning",
                      plotOutput("2DHotspotCluster")
                    ),
                    box(
                      title = "3D kMean Clustering", width = 4,height = 530, status = "warning",
                      rglwidgetOutput("3DHotspotCluster")
                    )
                  )
              )
            )
    )
    
  )
)


# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(disable = TRUE),
  sidebar,
  body
)




