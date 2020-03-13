

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sp)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(RODBC)
library(DT)
library(lmodel2)
library(zoo)
library(DBI)
library(odbc)
library(gtools)
library(rgdal)
library(plotly)

units <- c("ppm","pct")
trace <- c("Assay","Geology")

dbHeader <- dashboardHeader(title = "Drillhole Spatial Validation",
                            titleWidth = 300,
                            tags$li(a(href = 'https://www.shinyapps.io/',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.atamaianalytics.com',
                                      img(src = 'AtamaiAnalytics.jpeg',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

# Shinydashboard app
ui <- dashboardPage(skin="green",
                    dbHeader,
                    dashboardSidebar(width=300,
                                     sidebarMenu(
                                       h4(HTML(paste0("<b>","Dashboard Tabs","</b>"))),
                                       menuItem("Drillhole & Trace Selection", tabName = "dashboard", icon = icon("globe")),
                                       br(),
                                       menuItem("3D", tabName = "3d", icon = icon("cube")),
                                       hr(),
                                       h4(HTML(paste0("<b>","Instructions","</b>"))),
                                        HTML(" (1) <br/> Select a Project then GO to fetech, and display, <br/>
                                             drillholes associated with the Project. Zoom <br/> into the area of interest"),
                                      br(),
                                      br(),
                                        HTML(" (2) <br/> Use drawing tools, on the left side of the map,  
                                              to <br/> highlight a selection of drillholes."),
                                      br(),
                                      br(),
                                        HTML(" (3) <br/> Select an element, and prefered units, to <br/> calculate for drillhole traces."),
                                      br(),
                                      br(),
                                        HTML(" (4) <br/> Select a geological attribute to calculate for <br/> drillhole traces."),
                                      br(),
                                      br(),
                                        HTML(" (5) <br/> Select either Assay or Geology to display on the <br/> drillhole traces."),
                                      br(),
                                      br(),
                                        HTML(" (6) <br/> Select GO to generate drillhole traces."),
                                      br(),
                                      br(),
                                      HTML(" (7) <br/> Toggle Display Trace to view another attribute."),
                                      br(),
                                      br(),
                                      HTML(" (8) <br/> Select the 3D tab to view the selection in 3D."),
                                       hr(),
                                      h4(HTML(paste0("<b>","Disclaimer","</b>"))),
                                       "Do not use the results of this tool in public",
                                       br(),
                                       "reporting without an independent",
                                       br(),
                                       "verification and validation."
                                     )
                    ),

                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                      fluidRow(
                        column(width = 2,
                               box(background = "green",solidHeader = TRUE,width = NULL,
                                 h4("Select Drillholes",style="font-weight:bold"),
                                 hr(),
                                 uiOutput("project"),
                                 actionButton("getdata", "GO",style = "color: white;background-color: #35e51d")
                               ),
                               box(background = "green",solidHeader = TRUE,width = NULL,
                                 h4("Drill Trace & Attribute Selections",style="font-weight:bold"),
                                 hr(),
                                 fluidRow(
                                   column(6,uiOutput("element")),
                                   column(6,selectInput('Unit', 'Units',units,selected=""))
                                   ),
                                 hr(),
                                 fluidRow(
                                   column(8,uiOutput("geoattrib"))
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(8,selectInput('Trace', 'Display Trace',trace,selected=""))
                                 ),
                                 br(),
                                 actionButton("desurvey", "GO",style = "color: white;background-color: #35e51d")
                               )

                        ),
                        
                        column(width = 5,
                               box(width = NULL,
                                 leafletOutput("mymap")
                                 
                               ),
                               box(width = NULL,
                                   uiOutput("looknorthPlot")
                               )
                        ),
                        
                        column(width = 5,
                               box(width = NULL,
                                   uiOutput("planviewPlot")
                               ),
                               box(width = NULL, 
                                   uiOutput("lookeastPlot")
                               )
                        )
                      )
                    ),
                     tabItem(tabName = "3d",
                             plotlyOutput("threeD",
                                          height = "800px",
                                          width = "1000px") %>% withSpinner(type=4,color="#35e51d")
                     )
                    )
                    )

)