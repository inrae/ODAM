
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # Import global variables into this session
    gv <- globvars

    #----------------------------------------------------
    # Init
    #----------------------------------------------------
    source("Rsrc/libs.R", local=TRUE)          # Load libraries
    source("Rsrc/Init_UI.R", local=TRUE)       # UI Initialisation
    source("Rsrc/Plot_Info.R", local=TRUE)     # Dataset Information
    source("Rsrc/Plot_Uni.R", local=TRUE)      # Univariate
    source("Rsrc/Plot_Scatter.R", local=TRUE)  # Bivariate
    source("Rsrc/Plot_MultiUni.R", local=TRUE) # Multi-Univariate
    source("Rsrc/Plot_Multi.R", local=TRUE)    # Multivariate

    # Reactive values for error management
    ERROR <- reactiveValues(MsgErrorMain='', MsgErrorInfo='',  MsgErrorUni='', MsgErrorBi='', 
                            MsgErrorMuni='', MsgErrorMulti='', MsgErrorAbout='' )

    # Reactive values for UI
    values <- reactiveValues(
        # Init events
        init=0, initcol=0, initds=0, initdss=0, launch=0, nods=0,
        # uniplot events
        ttest=0,
        # multiplot events
        multitype='', outtype='', netData=NULL, ellipse=FALSE,
        # Error events
        error=0
    )

    # Shortcut for client session data
    cdata <- session$clientData

    # Session identifier
    SESSID <- paste(sample(c(0:9, letters[1:6]),15, replace=TRUE),collapse="")

    # Session temporary directory
    if (gv$saveplots) {
       SESSTMPDIR <- file.path(getwd(),'www/tmp',SESSID)
       dir.create(SESSTMPDIR, showWarnings = FALSE)
    } else {
       SESSTMPDIR <- tempdir()
    }
})
