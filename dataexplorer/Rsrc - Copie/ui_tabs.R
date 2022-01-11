#----------------------------------------------------
# Collection
#----------------------------------------------------
ui_collection <- tabItem(tabName = "collection", bsAlert("ErrAlertInfo"),
   box(title="Data Collection", status = "primary", solidHeader = TRUE, width = 12, collapsible = FALSE,
     conditionalPanel(condition="uiloaded==0",
        h3( tags$p('Please wait while loading ...'), tags$img(src = "loading.gif"), style = "color:#bf6f85")
     ),
     conditionalPanel(condition="uiloaded==1",
         #tags$hr(), verbatimTextOutput('out1'),
         htmlOutput("colinfos", class="mddiv"),
         tags$hr(), dataTableOutput("datasets"),
         tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keyEvent", e.which); });')
     )
   )
)

#----------------------------------------------------
# Information
#----------------------------------------------------
ui_infoTab <- tabItem(tabName = "information", bsAlert("ErrAlertInfo"),
   box(title="Dataset", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
     conditionalPanel(condition="uiloaded==0",
        h3( tags$p('Please wait while loading ...'), tags$img(src = "loading.gif"), style = "color:#bf6f85")
     ),
     conditionalPanel(condition="uiloaded==1",
        htmlOutput("datainfos", class="mddiv")
     )
   ),
   conditionalPanel(condition="output.apierror==0",
      box(
         title="Metadata", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
         dataTableOutput("metadata")
      ),
      box(
         title="Data Subsets", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
         dataTableOutput("subsets"),
         conditionalPanel(condition="output.dwnButton>0",
             tags$div(style="white-space: nowrap;", column(12,
                 downloadButton('downloadTSV', label = "Export the selected data subsets as a merged one", class = NULL),
                 tags$div(style="display: none", name="downldButton", tags$img(src='busy.gif', height = 20, width = 30))
             ))
         )
      )
   )
)

#----------------------------------------------------
# Intersection
#----------------------------------------------------
ui_intersection <- tabItem(tabName = "intersection", bsAlert("ErrAlertInfo"),
   conditionalPanel(condition="output.apierror==0",
      box(
         title="Data Graph", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
         diagonalNetworkOutput("Net", width="75%", height="600px"),
         tags$p(tags$i("Note: the number of quantitative variables is given in parentheses"))
      ),
      box(
         title="Data subset intersection", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
         column(6, selectInput("interSubset", "Data subset giving the identifier used as reference for distinct intersections", c())),
         # PLOTS
         column(12, conditionalPanel(condition="!(input.interSubset == 0)",
            plotOutput("UpSetPlot", height="500px") %>% withSpinner(color="brown")
         ))
      ),
      conditionalPanel(condition="output.DSsize>1 && output.DSsize<5 && output.nbvarsEvent==0", 
         box(
            title="Intersection of the selected data subsets", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
            plotOutput("VennPlot", height="500px") %>% withSpinner(color="brown")
         )
      ),
      div(class='div-session', box(
         title="Rodam session example", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
         tags$pre(tags$code(id="sessioninfo", class="language-r shiny-text-output ")),
         conditionalPanel(condition="output.hidekey==0", 
            bsButton("cp2clb", icon=icon("clipboard"), label = "Copy to clipboard", style="default", type="action")
         )
      ))
   )
)

ui_warning <- h3(tags$img(height = 50, width = 50, src = "https://www.freeiconspng.com/uploads/status-warning-icon-png-29.png"),
         em("This data subset is too large to be explored in an interactive way!"),br(),br(),
         em("Either upload the data subset or use the Rodam package (see 'About' tab)"),
         style = "color:#bf6f85")

#----------------------------------------------------
# Data table
#----------------------------------------------------
ui_dataTab <-  tabItem(tabName = "datatable", bsAlert("ErrAlertDT"), conditionalPanel(condition="output.apierror==0", 
   column(12,
      conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==0",
        tags$style(type='text/css', ".col-sm-11 { width: 90%; } .col-sm-1 { width: 10%; }"),
        column(1, 
           tags$style(type='text/css', "#show_vars .shiny-options-group label span { font-size: 11px; font-weight: 600; }"),
           checkboxGroupInput('show_vars', 'Columns to show:', choices = NULL, selected = NULL)
        ),
        column(11, DT::dataTableOutput('datavalues') )
      ),
      conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==1", ui_warning),
      conditionalPanel(condition="output.DSsize==0",
        h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
      )
   )
))

#----------------------------------------------------
# About
#----------------------------------------------------

ui_aboutTab <-  tabItem("about", bsAlert("ErrAlertAbout"),
   box(
      title="About", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
      htmlOutput("aboutinfos", class="mddiv")
   )
)

#----------------------------------------------------
# Univariate : BoxPlot 
#----------------------------------------------------
ui_uniTab <- tabItem(tabName = "univariate", bsAlert("ErrAlertUni"), conditionalPanel(condition="output.apierror==0", box(
   title="Univariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==0",
      # UP DIV
      div(class='div-top', column(12,
         column(4,
              selectInput("uniFacX", "Factor for X Axis", c() ),
              selectInput("SelFacX", "Select First Factor Levels", c(), multiple = TRUE, selectize=TRUE )
         ),
         column(4,
              selectInput("uniFacY", "Factor for Grouping", c() ),
              selectInput("SelFacY", "Select Second Factor Levels", c(), multiple = TRUE, selectize=TRUE )
         ),
         column(4,
              selectInput("uniVarSelect", "Variable to explore", c() ),
              column(4,
                 checkboxInput('uniSmooth', 'Curve', TRUE),
                 checkboxInput('uniLog', 'Log10', FALSE)
              ),
              column(4, conditionalPanel(condition="input.uniFacX == input.uniFacY", 
                    checkboxInput('violin', 'Violin', FALSE),
                    checkboxInput('ttest', 'Paired Test', FALSE)
                 )
              ),
              column(4, conditionalPanel(condition="input.uniFacX == input.uniFacY && input.ttest==1", 
                     selectInput("ttestType", "", c("T-test" = "t.test", "Wilcoxon"="wilcox.test"), selected = "t.test")
                 )
              )
         )
      )),
      # PLOTS
      column(12,
         conditionalPanel(condition="input.uniVarSelect>0", 
              conditionalPanel(condition="input.ttest==1", imageOutput("TtestPlot", height="500px") %>% withSpinner(color="brown")),
              conditionalPanel(condition="input.ttest==0", plotlyOutput("BoxPlot", height="500px") %>% withSpinner(color="brown"))
         )
      ),
      column(12, p("")),
      # DOWN DIV
      div(class='div-down', column(12,
         column(4,
              selectInput("uniAnnot", "Select Data based on Features", c() )
         ),
         column(8,
              selectInput("uniFeatures", "(Un)Select Features", c(), multiple = TRUE, , selectize=TRUE )
         )
      ))
   ),   
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==1", ui_warning),
   conditionalPanel(condition="output.DSsize==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
)))

#----------------------------------------------------
# Bivariate : ScatterPlot 
#----------------------------------------------------
ui_scatterTab <- tabItem(tabName = "bivariate", bsAlert("ErrAlertBi"),conditionalPanel(condition="output.apierror==0", box(
   title="Bivariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==0",
      # UP DIV
      div(class='div-top', column(12,
         column(4,
              selectInput("biFacX", "Factor for Grouping", c() )
         ),
         column(4,
              selectInput("biVarSelect1", "First Variable", c() )
         ),
         column(4,
              selectInput("biVarSelect2", "Second Variable", c() )
         )
      ),
      column(12,
         column(4,
              selectInput("SelFacX2", "Select First Factor Levels", c(), multiple = TRUE, , selectize=TRUE )
         ),
         column(4, tags$p("") ),
         column(4,
              column(6,
                 radioButtons("biAddon", "Add On:", c("None" = "none", "Reg. model (LM)" = "regmod", "Ellipse" = "ellipse", "Smooth"="smooth"), selected="ellipse")
              ),
              column(6,
                  checkboxInput('biLabels', 'Labels', TRUE),
                  checkboxInput('biLogX', 'Log10(X)', FALSE),
                  checkboxInput('biLogY', 'Log10(Y)', FALSE),
                  checkboxInput('gregmod', 'Fit Lin. Model', FALSE)
              )
         )
      )),
      # PLOTS
      column(12, conditionalPanel(condition="input.biVarSelect1>0 && input.biVarSelect2>0", 
         plotlyOutput("ScatterPlot", height="500px")  %>% withSpinner(color="brown")
      )),
      column(12, p("")),
      # DOWN DIV
      div(class='div-down', column(12,
         column(4,
              selectInput("biAnnot", "Select Features as Labels", c() )
         ),
         column(8,
              selectInput("biFeatures", "(Un)Select Items", c(), multiple = TRUE, , selectize=TRUE )
         )
      ))
   ),
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==1", ui_warning),
   conditionalPanel(condition="output.DSsize==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
)))

#----------------------------------------------------
# Multi-univariate : VolcanoPlot 
#----------------------------------------------------
ui_multiuniTab <- tabItem(tabName = "multiunivariate", bsAlert("ErrAlertMuni"), conditionalPanel(condition="output.apierror==0", box(
   title="Volcano Plot", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==0",
      # UP DIV
      div(class='div-top', column(12,
         column(4,
              selectInput("muniFacX", "Factor for X Axis", c() ),
              column(12,
                 column(6, selectInput("ptype", "Method", c("T-test"="0", "Wilcoxon"="1"))),
                 column(6, selectInput("tadj", "Adj. p-values",
                           c("None"="none", "Holm"="holm","Benjamini-Hochberg"="BH", "Benjamini-Yekutieli"="BY", "Bonferroni"="bonf"), selected="BH"))
              )
         ),
         column(4,
              selectInput("SelLev1", "Reference level", c() ),
              column(12,
                 column(6, numericInput("pval", "P-value threshold", 0.05, min = 0.01, max = 0.1, step=0.01 )),
                 column(6, numericInput("foldChange", "FoldChange threshold", 1, min = 1, max = 10, step=0.5))
              )
         ),
         column(4,
              selectInput("SelLev2", "Level to be compared", c() ),
              column(12,
                 column(6, selectInput("onlytop", "Labels", c("All"="0", "Only Top 5"="5", "Only Top 10"="10", "Only Top 25"="25"), selected="10" ))
              )
         )
      )),
      # PLOTS
      column(12, conditionalPanel(condition="input.SelLev1>0 && input.SelLev2>0 && input.SelLev1!=input.SelLev2", 
         plotlyOutput("volcanoPlot", height="600px")  %>% withSpinner(color="brown")
      )),
      column(12, p(""))
   ),   
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==1", ui_warning),
   conditionalPanel(condition="output.DSsize==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
)))

#----------------------------------------------------
# Multivariate : PCA / ICA / COR / GGM
#----------------------------------------------------
ui_multiTab <- tabItem(tabName = "multivariate", bsAlert("ErrAlertMulti"), conditionalPanel(condition="output.apierror==0", box(
   title="Multivariate exploration", status = "primary", solidHeader = TRUE, width = 12,
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==0",
      htmlOutput("Msg"),
      # UP DIV
      div(class='div-top', column(12,
         column(4,
              selectInput("multiFacX", "Factor for highlighting the classification", c() ),
              conditionalPanel(condition="input.multiType=='PCA' || input.multiType=='ICA' || input.multiType=='TSNE'",
              column(12,
                   column(6, checkboxInput('ellipse', 'Ellipses', TRUE)),
                   column(6, selectInput("conflevel", NULL, c("0.8"="0.8", "0.9"="0.9", "0.95"="0.95", "0.99"="0.99"), selected="0.95" ))
              )),
              selectInput("listLevels", "Select Factor Levels", c(), multiple = TRUE, , selectize=TRUE )
         ),
         column(4,
              selectInput("multiType", "Analysis Type", 
                     c("----"="None"  , "Principal Component Analysis (PCA)" = "PCA" 
                                      , "Independent Component Analysis (ICA)" = "ICA" 
                                      , "t-Distributed Stochastic Neighbor Embedding (t-SNE)" = "TSNE"
                                      , "Heatmap of correlation matrix (COR)" = "COR" 
                                      , "Gaussian graphical model (GGM)" = "GGM"
                                      ), selected = "None"),
      
              conditionalPanel(condition="input.multiType=='PCA' || input.multiType=='ICA' || input.multiType=='TSNE'",
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
                       ),
                       conditionalPanel(condition="input.multiType=='TSNE'",
                       column(12,
                            column(4,  HTML('<b>Perplexity</b>')),
                            column(8, numericInput("perplexity", NULL, 15, min = 5, max = 50, step=5))
                       ))
                   )
              ),
              conditionalPanel(condition="input.multiType=='COR'", 
                   column(12,
                       column(2,  HTML('<b>Correlation type</b>')),
                       column(4,
                            selectInput("methcor", NULL, c("Pearson"="pearson", "Spearman"="spearman", "Kendall"="kendall", selected="pearson" ))
                       )
                   )
              ),
              conditionalPanel(condition="input.multiType=='GGM'", 
                   column(6,
                       column(3,  HTML('<b>FDR qvalue</b>')),
                       column(9, numericInput("qval", NULL, 0.05, min = 0.001, max = 0.1, step=0.01) )
                   ),
                   column(6,
                       column(3,  HTML('<br><b>Gravity</b>')),
                       column(9,  sliderInput("gravite", NULL, min = 0.3, max = 0.7, value = 0.5))
                   )
              )
         ),
         column(4,
              selectInput("outType", "Output Type", 
                   c("----"="None", "Identifiers" = "IDS", "Variables" = "VARS"), selected = "None"),
              conditionalPanel(condition="input.multiType=='PCA' || input.multiType=='ICA' || input.multiType=='TSNE'",
                  column(2, checkboxInput('f3D', '3D', FALSE)),
                  column(2, checkboxInput('multiLabels', 'Labels', TRUE)), 
                  column(3, checkboxInput('shortLabels', 'Short Labels', FALSE))
              ),
              conditionalPanel(condition="input.multiType=='COR'",
                  column(4, checkboxInput('fullmatcor', 'Full Matrix', TRUE)),
                  column(4, checkboxInput('reordermatcor', 'Reorder Matrix', TRUE))
              ),
              conditionalPanel(condition="input.multiType=='GGM'",
                 column(4, checkboxInput('shrinkauto', 'Shrinkage Auto', TRUE)),
                 column(4, conditionalPanel(condition="input.shrinkauto==0", 
                       numericInput("lambda", NULL, 0.3, min = 0.0001, max = 1, step=0.1)
                 ))
              ),
              conditionalPanel(condition="input.multiType=='GGM' || input.multiType=='COR'",
                 column(4, checkboxInput('multiLog', 'Log10', FALSE))
              )
         )
      )),
      # PLOTS
      column(12,
        conditionalPanel(
            condition="output.nbvarsEvent2==1 && (input.multiType=='GGM' || input.multiType=='COR')", 
                   h3(tags$img(height = 50, width = 50, src = "https://www.freeiconspng.com/uploads/status-warning-icon-png-29.png"),
                      em("Too many variables for an understandable graph!"),br(),br(),
                      em("Either upload the data subset or use the Rodam package (see 'About' tab)"),
                      style = "color:#bf6f85")
         ),
         conditionalPanel(condition="input.multiType != 'None' && input.outType != 'None' && input.listVars[2]", 
            conditionalPanel(condition="input.multiType=='PCA' || input.multiType=='ICA' || input.multiType=='TSNE'", 
                    plotlyOutput("MultiPlot", height="600px") %>% withSpinner(color="brown")),
            conditionalPanel(condition="input.multiType=='COR'", 
                    imageOutput("CorrPlot", height="600px") %>% withSpinner(color="brown")),
            conditionalPanel(condition="input.multiType=='GGM'", 
                    forceNetworkOutput("ggmnet", width="85%", height="800px") %>% withSpinner(color="brown")),
            uiOutput('urlimage')
         )
      ),
      column(12, p("")),
      # DOWN DIV
      div(class='div-down', column(12,
         column(4,
              selectInput("multiAnnot", "Select Data based on Features", c() )
         ), 
         column(8,
              selectInput("listFeatures", "(Un)Select Features", c(), multiple = TRUE, , selectize=TRUE )
         )
      ),
      column(12,
          selectInput("listVars", "Select Variables", c(), multiple = TRUE, , selectize=TRUE )
      ))
    ),
   conditionalPanel(condition="output.DSsize>0 && output.nbvarsEvent==1", ui_warning),
   conditionalPanel(condition="output.DSsize==0",
      h3(em("Please, select a Data Subset in the corresponding Drop List above"), style = "color:#a2a2bb")
   )
)))

