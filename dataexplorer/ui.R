library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(networkD3)
#library(plotly)

source("R/ui_tabs.R", local=TRUE)

meta <- tags$head(
HTML('
<meta http-equiv="cache-control" content="max-age=0" />
<meta http-equiv="cache-control" content="no-cache" />
<meta http-equiv="expires" content="0" />
<meta http-equiv="expires" content="Tue, 01 Jan 1980 1:00:00 GMT" />
<meta http-equiv="pragma" content="no-cache" />
'),
tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
# Load D3.js
tags$script(src = 'http://d3js.org/d3.v3.min.js')
)

ui <- dashboardPage(skin = "blue",

  dashboardHeader(title = "ODAM - Data Explorer", titleWidth = 450, disable = FALSE ),

  dashboardSidebar(
    #----------------------------------------------------
    # Sidebar Menu
    #----------------------------------------------------
        h2(textOutput("datasetname"), align = "center"),
        selectInput("inDSelect", "Select a Data Subset", c() ),
        sidebarMenu(
            id="IdMenu",
            menuItem("Subset Information",   tabName = "information",   icon = icon("eye")),
            menuItem("Univariate",   tabName = "univariate",   icon = icon("bar-chart")),
            menuItem("Bivariate",    tabName = "bivariate",    icon = icon("bar-chart")),
            menuItem("Multivariate", tabName = "multivariate", icon = icon("bar-chart")),
            menuItem("About",   tabName = "about",   icon = icon("info-circle"))
        )
  ),

  dashboardBody(
    meta,
    useShinyjs(),
    inlineCSS(list(.hideElem = "display: none")),
    extendShinyjs("js/app.js"),
    fluidRow(
      tabItems(
      #----------------------------------------------------
      # Information
      #----------------------------------------------------
         ui_infoTab,
      #----------------------------------------------------
      # Univariate : BoxPlot 
      #----------------------------------------------------
         ui_uniTab,
      #----------------------------------------------------
      # Bivariate : ScatterPlot 
      #----------------------------------------------------
         ui_scatterTab,
      #----------------------------------------------------
      # Multivariate : PCA / ICA
      #----------------------------------------------------
         ui_multiTab,
      #----------------------------------------------------
      # About
      #----------------------------------------------------
         ui_aboutTab
      )
    )
  )
)
