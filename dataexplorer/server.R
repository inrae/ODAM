
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #----------------------------------------------------
    # Init
    #----------------------------------------------------
    source("Rsrc/libs.R", local=TRUE)          # Load libraries
    source("Rsrc/Init_UI.R", local=TRUE)       # UI Initialisation
    source("Rsrc/Plot_Info.R", local=TRUE)     # Dataset Information
    source("Rsrc/Plot_Uni.R", local=TRUE)      # Univariate
    source("Rsrc/Plot_Scatter.R", local=TRUE)  # Bivariate
    source("Rsrc/Plot_Multi.R", local=TRUE)    # Multivariate

    ERROR <- reactiveValues(MsgErrorMain='', MsgErrorInfo='', MsgErrorDT='', MsgErrorUni='', 
                            MsgErrorBi='', MsgErrorMulti='', MsgErrorAbout='' )

    cdata <- session$clientData
    values <- reactiveValues(init=0, initcol=0, initds=0, launch=0, error=0, netData=NULL)

    nbopt_multiselect <- 20

})
