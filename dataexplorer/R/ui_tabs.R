#----------------------------------------------------
# Information
#----------------------------------------------------
ui_infoTab <- tabItem(tabName = "information",
   box(
      title="Data Subset Information", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      HTML("<div id='wait' class='shiny-html-output'><table><tr><td><img src='loading.gif' height=25 width=40 /></td><td>&nbsp;</td><td>Loading ...</td></tr></table></div>"),
      #verbatimTextOutput('out1'),
      dataTableOutput("subsets"),
      conditionalPanel(condition="input.inDSelect>0", downloadButton('downloadTSV', label = "Export TSV", class = NULL), p(), dataTableOutput("infos"))
   ),
   box(
      title="Data Graph", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      diagonalNetworkOutput("Net", width="75%", height="600px")
   )
)

#----------------------------------------------------
# Data table
#----------------------------------------------------
ui_dataTab <-  tabItem(tabName = "datatable",
   column(12,
      conditionalPanel(condition="input.inDSelect>0",
        column(1, 
           checkboxGroupInput('show_vars', 'Columns to show:', choices = NULL, selected = NULL)
        ),
        column(11, DT::dataTableOutput('datavalues') )
      ),
      conditionalPanel(condition="input.inDSelect==0",
        h3(em("Please, select a Data Subset in the Drop List on the left sidebar "), style = "color:#a2a2bb")
      )
   )
)

#----------------------------------------------------
# About
#----------------------------------------------------

ui_aboutTab <-  tabItem("about",  
   box(
      title="About", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      img(src="odam-logo.png", height = 100), br(),
      column(10, 
         p( shiny::icon("info-circle"), " Created by Daniel Jacob - INRA UMR 1332 BFP, PMB - 2016 - Version ", idVersion, style = "color:#2121d2; font-size: 120%"), 
         hr(),
         p( "Give an open access to your data and make them ready to be mined - A data explorer as bonus", style = "font-size: 120%; line-height: 120%" ),
         tags$ul(
            tags$li("For this work, we were inspired by the ", a(href="https://www.google.com/publicdata/directory", target="_blank", "Google Public Data Explorer"), ". But instead of using their ", a(href="https://developers.google.com/public-data/overview", target="_blank", "Dataset Publishing Language (DSPL)"), " based on XML format, we made the choice to keep the good old way of scientist to use worksheets, thus using the same tool for both data files and metadata definition files. Moreover, unlike the Google approach, our approach gives data access through web-services thus providing a good way to connect distributed data. For more information/explanation,  see an", a(href="http://fr.slideshare.net/danieljacob771282/odam-open-data-access-and-mining", target="_blank", "online presentation"), style = "line-height: 160%"),
            tags$li( "To prepare your own data subsets, see the ", a( href="https://github.com/INRA/ODAM/blob/master/doc/tutorial_on_metadata_files.pdf", target="_blank", "tutorial on metadata files")),
            tags$li( "Test online the getData API through the web ", a( href="http://www.bordeaux.inra.fr/pmb/odamsw/", target="_blank", "swagger UI")), 
            tags$li( "For open data access throught web services within R, see the ", a( href="Rodam.html", target="_blank", "R ODAM package and How to use it")), 
            tags$li( "To install the ODAM software suite on your own harware (laptop or server), the Docker containerization software is required, 
                      either as a component directly installed on your system or embedded within a Virtual Machine. Get the docker images and the installation instruction to the ", 
                      a(href="https://hub.docker.com/r/odam/getdata/", target="_blank", "Dockker Hub"), " site.", style = "line-height: 160%" ),
            tags$li( "Test online with the ", a( href="?ds=frim1", "FRIM dataset"), " - ",
                     HTML("<a href='https://doi.org/10.5281/zenodo.154041' target='_blank'><img src='https://zenodo.org/badge/DOI/10.5281/zenodo.154041.svg' alt='DOI'></a>") )
         ,  style = "font-size: 120%; line-height: 240%" )
      )
   ),
   box(
      title="Session Information", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      tags$pre(tags$code(id="sessioninfo", class="language-r shiny-text-output "))
   )
)

#----------------------------------------------------
# Univariate : BoxPlot 
#----------------------------------------------------
ui_uniTab <- tabItem(tabName = "univariate", box(
   title="Univariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="input.inDSelect>0",
      column(4,
           selectInput("uniFacX", "Factor for X Axis", c() )
      ),
      column(4,
           selectInput("uniFacY", "Factor for Grouping", c() )
      ),
      column(4,
           selectInput("uniVarSelect", "Variable to explore", c() )
      ),
      column(12, conditionalPanel(condition="input.uniVarSelect>0", 
            plotlyOutput("BoxPlot", height="500px") )
      ),
      column(4,
           selectInput("SelFacX", "Select First Factor Levels", c(), multiple = TRUE, selectize=TRUE )
      ),
      column(4, conditionalPanel(condition="input.uniFacX != input.uniFacY", 
           selectInput("SelFacY", "Select Second Factor Levels", c(), multiple = TRUE, selectize=TRUE ))
      ),
      column(4,
           checkboxInput('uniSmooth', 'Curve', TRUE),
           checkboxInput('uniLog', 'Log10', FALSE)
      )
   ),
   conditionalPanel(condition="input.inDSelect==0",
      h3(em("Please, select a Data Subset in the Drop List on the left sidebar "), style = "color:#a2a2bb")
   )
))

#----------------------------------------------------
# Bivariate : ScatterPlot 
#----------------------------------------------------
ui_scatterTab <- tabItem(tabName = "bivariate", box(
   title="Bivariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="input.inDSelect>0",
      column(4,
           selectInput("biFacX", "Factor for Grouping", c() )
      ),
      column(4,
           selectInput("biVarSelect1", "First Variable", c() )
      ),
      column(4,
           selectInput("biVarSelect2", "Second Variable", c() )
      ),
      column(12, conditionalPanel(condition="input.biVarSelect1>0 && input.biVarSelect2>0", 
            plotlyOutput("ScatterPlot", height="500px")
      )),
      column(4,
           selectInput("SelFacX2", "Select First Factor Levels", c(), multiple = TRUE, , selectize=TRUE )
      ),
      column(4,
           selectInput("biAnnot", "Select Features as Labels", c() )
           , selectInput("biFeatures", "(Un)Select Items", c(), multiple = TRUE, , selectize=TRUE )
      ),
      column(4,
           column(6,
              radioButtons("biAddon", "Add On:", c("None" = "none", "Regression model (LM)" = "regmod", "Ellipse" = "ellipse"), selected="ellipse")
           ),
           column(6,
               checkboxInput('biLabels', 'Labels', TRUE),
               checkboxInput('biLogX', 'Log10(X)', FALSE),
               checkboxInput('biLogY', 'Log10(Y)', FALSE)
           )
      )
   ),
   conditionalPanel(condition="input.inDSelect==0",
      h3(em("Please, select a Data Subset in the Drop List on the left sidebar "), style = "color:#a2a2bb")
   )
))

#----------------------------------------------------
# Multivariate : PCA / ICA
#----------------------------------------------------
ui_multiTab <- tabItem(tabName = "multivariate", box(
   title="Multivariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="input.inDSelect>0",
   htmlOutput("Msg"),
      column(4,
           selectInput("multiFacX", "Factor for highlighting the classification", c() ),
           checkboxInput('ellipse', 'Ellipses', TRUE)
      ),
      column(4,
           selectInput("multiType", "Analysis Type", 
                  c("----"="None"  , "Principal Component Analysis (PCA)" = "PCA" 
                                   , "Independent Component Analysis (ICA)" = "ICA" 
                                   ), selected = "None"),
           column(6,
                checkboxInput('scale', 'Scale', FALSE)
           ),
           column(6, 
                conditionalPanel(condition="input.multiType=='PCA'",
                    selectInput("viewComp", NULL, c("PC1 vs PC2"="1", "PC1 vs PC3"="2", "PC2 vs PC3"="3"), selected="1" )
                ),
                conditionalPanel(condition="input.multiType=='ICA'",
                    selectInput("nbComp", NULL, c("2 Components"="2", "3 Components"="3", "4 Components"="4", 
                                                  "5 Components"="5", "6 Components"="6" ), selected="2" )
                )
           )
      ),
      column(4,
           selectInput("outType", "Output Type", 
                c("----"="None", "Identifiers" = "IDS", "Variables" = "VARS"), selected = "None"),
           column(4, checkboxInput('f3D', '3D', FALSE)),
           column(4, checkboxInput('multiLabels', 'Labels', TRUE)), 
           column(4, checkboxInput('GBG', 'Grey Background', FALSE))
      ),
      column(12, conditionalPanel(condition="input.multiType != 'None' && input.outType != 'None' && input.listVars[2]", 
            plotlyOutput("MultiPlot", height="600px") )
      ),
      column(4,
           selectInput("listLevels", "Select Factor Levels", c(), multiple = TRUE, , selectize=TRUE )
      ), 
      column(4,
           selectInput("multiAnnot", "Select Features as Labels", c() )
           , selectInput("listFeatures", "(Un)Select Items", c(), multiple = TRUE, , selectize=TRUE )
      ), 
      column(4,
           selectInput("listVars", "Select Variables", c(), multiple = TRUE, , selectize=TRUE )
      )
   ),
   conditionalPanel(condition="input.inDSelect==0",
      h3(em("Please, select a Data Subset in the Drop List on the left sidebar "), style = "color:#a2a2bb")
   )
))
