
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #----------------------------------------------------
    # Init
    #----------------------------------------------------
    source("R/libs.R", local=TRUE)          # Load libraries
    source("R/Init_UI.R", local=TRUE)       # UI Initialisation
    source("R/Plot_Info.R", local=TRUE)     # Dataset Information
    source("R/Plot_Uni.R", local=TRUE)      # Univariate
    source("R/Plot_Scatter.R", local=TRUE)  # Bivariate
    source("R/Plot_Multi.R", local=TRUE)    # Multivariate

    ERROR <- reactiveValues(MsgErrorMain='', MsgErrorInfo='', MsgErrorDT='', MsgErrorUni='', MsgErrorBi='', MsgErrorMulti='', MsgErrorAbout='' )

    cdata <- session$clientData
    values <- reactiveValues(init=0, initcol=0, initds=0, launch=0, netData=NULL)

    nbopt_multiselect <- 20

})
