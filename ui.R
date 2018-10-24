#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bank Credit Marketing - Tall Machine Learning Group"),
  tabsetPanel(id ="dataTabSet",
              tabPanel("Report",
                       br()
                       # strong("For viewing html report, please click",
                       #        tags$a(href='report.html',' here', target='blank'), "."),
                       # br(),
                       # br(),
                       # includeMarkdown("report.Rmd")
              ),
              tabPanel("Exploring Toronto Criminals",
                       br(),
                       tabsetPanel(id="tabset",
                                   
                                   tabPanel("Map",
                                            br()
                                   ),
                                   
                                   tabPanel("Toronto Crimes",
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                              ),
                                              mainPanel(
                                                fluidRow(
                                                  column(4,
                                                         selectInput("neighbourhood",
                                                                     "Neighbourhood to explore:",
                                                                     "placeholder")),
                                                  column(4,
                                                         selectInput("typeOfCrime", 
                                                                     strong("Type of Crime"), 
                                                                     c("All", "Assault","Break and Enter", "Robbery", "Theft Over", "Auto Theft" ))),
                                                  column(4,
                                                         selectInput("occurredYear", 
                                                                            strong("Occurrence Year"), 
                                                                            choices = list("All", 2017, 2016, 2015, 2014),
                                                                            selected = "All"))
                                                  
                                                ),
                                                fluidRow(
                                                  column(6,
                                                         plotOutput("crimeTime")),
                                                  column(6,
                                                         plotOutput("crimeDayOfWeek"))
                                                    
                                                ),
                                                br(),
                                                br(),
                                                fluidRow(
                                                  column(6,
                                                         plotOutput("crimeMonth")
                                                    
                                                  ),
                                                  column(6,
                                                         DT:: dataTableOutput("neighbourhoodTable"))
                                                )
                                              )
                                                
                                          )
                                            
                                   
                       
                                   )
                       )
           )
        )
           
    )
           
)

