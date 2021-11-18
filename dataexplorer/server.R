
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
    values <- reactiveValues(
        # Init events
        init=0, initcol=0, initds=0, launch=0, nods=0,
        # multiplot events
        multitype='', outtype='', netData=NULL, multiload=0,
        # Error events
        error=0, 
    )

    # Session identifier
    SESSID <- paste(sample(c(0:9, letters[1:6]),15, replace=TRUE),collapse="")

    # Session temporary directory
    if (saveplots) {
       SESSTMPDIR <- file.path(getwd(),'www/tmp',SESSID)
       dir.create(SESSTMPDIR, showWarnings = FALSE)
    } else {
       SESSTMPDIR <- tempdir()
    }

    js$openTab("information")
    addCssClass(selector = "a[data-value='collection']", class = "inactiveItem")

})
