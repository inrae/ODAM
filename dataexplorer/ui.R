source("Rsrc/ui_tabs.R", local=TRUE)

meta <- tags$head(
   HTML('
   <meta http-equiv="cache-control" content="max-age=0" />
   <meta http-equiv="cache-control" content="no-cache" />
   <meta http-equiv="expires" content="0" />
   <meta http-equiv="expires" content="Tue, 01 Jan 1980 1:00:00 GMT" />
   <meta http-equiv="pragma" content="no-cache" />
   '),
   tags$script(type="text/javascript", src = "js/google-analytics.js"),
   tags$script(type="text/javascript", src = "js/utils.js"),
   tags$link(rel = "stylesheet", type = "text/css", href = paste0(globvars$theme,".css"))
)

busyLogo <- function(busysrc, height = 30, width = 30, alt = NULL) {
  tagList(
    tags$head(
      tags$script("setInterval(function(){if ($('html').attr('class')=='shiny-busy') { $('img.busy').show(); } else { $('img.busy').hide(); } },100)")
    ),
    img(class = "busy", src=busysrc,height = height, width = width, alt = alt)
  )
}

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "ODAM - Data Explorer", disable = FALSE, # titleWidth = 350, 
         # Collection / Dataset name
            tags$li(h2(textOutput("datasetname"), align = "center"), class = "dropdown"),
            tags$li(img(src="img_00.gif",height = 10, width = 10), class = "dropdown"),
         # Dataset selection
            tags$li(selectInput("inDselect", "", c() ), class = "dropdown inDselect"),
            tags$li(img(src="img_00.gif",height = 10, width = 5), class = "dropdown"),
         # Data subset selection
            tags$li(selectInput("inDSselect", "", c(), multiple = TRUE, width="600px" ), class = "dropdown inDSselect"),
            tags$li(img(src="img_00.gif",height = 10, width = 20), class = "dropdown"),
         # Busy logo
            tags$li(busyLogo('busy.gif'), class = "dropdown")
  ),

  dashboardSidebar(
    #----------------------------------------------------
    # Sidebar Menu
    #----------------------------------------------------
        tags$br(),
        sidebarMenu(
            id="IdMenu",
            menuItem("Collection",   tabName = "collection",   icon = icon("eye")),
            menuItem("Subset Information",   tabName = "information",   icon = icon("eye")),
            menuItem("Data Table",   tabName = "datatable",   icon = icon("bar-chart")),
            menuItem("Univariate",   tabName = "univariate",   icon = icon("bar-chart")),
            menuItem("Bivariate",    tabName = "bivariate",    icon = icon("bar-chart")),
            menuItem("Multi-univariate", tabName = "multiunivariate", icon = icon("bar-chart")),
            menuItem("Multivariate", tabName = "multivariate", icon = icon("bar-chart")),
            menuItem("About",   tabName = "about",   icon = icon("info-circle"))
        ), collapsed = TRUE
  ),

  dashboardBody(
    meta,
    shinyjs::useShinyjs(debug = globvars$debug_shinyjs, html = FALSE),
    shinyjs::inlineCSS(list(.hideElem = "display: none")),
    shinyjs::inlineCSS(list(.dataTables_info="display: inline")),

    shinyjs::extendShinyjs(script="js/app.js", 
              functions = c("hideMainHeader", "hideSidebar", "showSidebar", "hideSidebarToggle", 
                            "hideinDselect", "showinDselect", "hideinDSselect", "showinDSselect", 
                            "openTab")),

    fluidRow(
      tags$script("var uiloaded=0; var ipclient=''; var apikey='';"),
      bsAlert("ErrAlertMain"),
      tabItems( 
      #----------------------------------------------------
      # Collection
      #----------------------------------------------------
         ui_collection,
      #----------------------------------------------------
      # Information
      #----------------------------------------------------
         ui_infoTab,
      #----------------------------------------------------
      # Data table
      #----------------------------------------------------
         ui_dataTab,
      #----------------------------------------------------
      # Univariate : BoxPlot 
      #----------------------------------------------------
         ui_uniTab,
      #----------------------------------------------------
      # Bivariate : ScatterPlot 
      #----------------------------------------------------
         ui_scatterTab,
      #----------------------------------------------------
      # Multi-Univariate : VolcanoPlot 
      #----------------------------------------------------
         ui_multiuniTab,
      #----------------------------------------------------
      # Multivariate : PCA / ICA
      #----------------------------------------------------
         ui_multiTab,
      #----------------------------------------------------
      # About
      #----------------------------------------------------
         ui_aboutTab
      ),
      textInput("ipclient", "", "")

    )
  )
)
