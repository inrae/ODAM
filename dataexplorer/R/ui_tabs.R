#----------------------------------------------------
# Information
#----------------------------------------------------
ui_infoTab <- tabItem(tabName = "information", bsAlert("ErrAlertInfo"),
   box(title="Data Information", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
     htmlOutput("datainfos", class="mddiv")
   ),
   box(
      title="Data Subset Information", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      HTML("<div id='wait' class='shiny-html-output'><table><tr><td><img src='loading.gif' height=25 width=40 /></td><td>&nbsp;</td><td>Loading ...</td></tr></table></div>"),
      #verbatimTextOutput('out1'),
      dataTableOutput("subsets"),
      conditionalPanel(condition="input.inDSselect>0", downloadButton('downloadTSV', label = "Export TSV", class = NULL), p(), dataTableOutput("infos"))
   ),
   box(
      title="Data Graph", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      diagonalNetworkOutput("Net", width="75%", height="600px")
   )
)

#----------------------------------------------------
# Data table
#----------------------------------------------------
ui_dataTab <-  tabItem(tabName = "datatable", bsAlert("ErrAlertDT"),
   column(12,
      conditionalPanel(condition="input.inDSselect>0",
        tags$style(type='text/css', ".col-sm-11 { width: 90%; } .col-sm-1 { width: 10%; }"),
        column(1, 
           tags$style(type='text/css', "#show_vars .shiny-options-group label span { font-size: 11px; font-weight: 600; }"),
           checkboxGroupInput('show_vars', 'Columns to show:', choices = NULL, selected = NULL)
        ),
        column(11, DT::dataTableOutput('datavalues') )
      ),
      conditionalPanel(condition="input.inDSselect==0",
        h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
      )
   )
)

#----------------------------------------------------
# About
#----------------------------------------------------

ui_aboutTab <-  tabItem("about", bsAlert("ErrAlertAbout"),
   box(
      title="About", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      htmlOutput("aboutinfos", class="mddiv")
   ),
   box(
      title="Session Information", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      tags$pre(tags$code(id="sessioninfo", class="language-r shiny-text-output "))
   )
)

#----------------------------------------------------
# Univariate : BoxPlot 
#----------------------------------------------------
ui_uniTab <- tabItem(tabName = "univariate", bsAlert("ErrAlertUni"), box(
   title="Univariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="input.inDSselect>0",
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
   conditionalPanel(condition="input.inDSselect==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
))

#----------------------------------------------------
# Bivariate : ScatterPlot 
#----------------------------------------------------
ui_scatterTab <- tabItem(tabName = "bivariate", bsAlert("ErrAlertBi"),box(
   title="Bivariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="input.inDSselect>0",
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
   conditionalPanel(condition="input.inDSselect==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
))

#----------------------------------------------------
# Multivariate : PCA / ICA
#----------------------------------------------------
ui_multiTab <- tabItem(tabName = "multivariate", bsAlert("ErrAlertMulti"), box(
   title="Multivariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="input.inDSselect>0",
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
                checkboxInput('scale', 'Scale', TRUE)
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
   conditionalPanel(condition="input.inDSselect==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
))

