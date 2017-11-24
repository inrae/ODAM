
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    #----------------------------------------------------
    # Init
    #----------------------------------------------------
    source("R/libs.R", local=TRUE)          # Load libraries
    source("R/Plot_Info.R", local=TRUE)     # Dataset Information
    source("R/Plot_Uni.R", local=TRUE)      # Univariate
    source("R/Plot_Scatter.R", local=TRUE)  # Bivariate
    source("R/Plot_Multi.R", local=TRUE)    # Multivariate

    ERROR <- reactiveValues(MsgErrorMain='', MsgErrorInfo='', MsgErrorDT='', MsgErrorUni='', MsgErrorBi='', MsgErrorMulti='', MsgErrorAbout='' )
    observe ({
       ERROR$MsgErrorMain
       if (nchar(ERROR$MsgErrorMain)>0) {
          createAlert(session, "ErrAlertMain", "ErrAlertMainId", title = "", content = ERROR$MsgErrorMain, append = FALSE, style='danger')
       }
    })

    cdata <- session$clientData
    values <- reactiveValues()
    values$init <- 0

    nbopt_multiselect <- 20

    observe({
       if (is.DS(cdata)) {
           tryCatch({
              ws <<-getWS(cdata)
              getInit()
              if (! is.null(ws[6]) && ws[6] %in% c('datatable','univariate','bivariate','multivariate') ) {
                 js$openTab(ws[6])
                 js$hideSidebar()
                 if (! is.null(ws[7]) && ws[7] %in% c('off') ) {
                    js$hideMainHeader()
                 }
              }
           }, error=function(e) { ERROR$MsgErrorMain <- paste("INIT: ", e ); })
       } else {
          isolate({updateTabItems(session, "IdMenu", "about")})
          js$hideSidebar()
          js$hideSidebarToggle()
          js$hideinDSelect()
       }
    })

    # Display the Dataset Name 
    output$datasetname <- renderText({ ws[2] })

    #----------------------------------------------------
    # dynamically building of the UI of each analysis tab depending on the selected data subset
    # ( based on the onChange event, linked to the 'inDSelect' input - See the 'R/Plot_XXX.R' files )
    #----------------------------------------------------
    observe({
        updateSelectInput(session, "inDSelect", choices = DSL, selected = inDSelect)
    })

})
