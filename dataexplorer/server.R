
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

    cdata <- session$clientData
    values <- reactiveValues()
    values$init <- 0

    nbopt_multiselect <- 20

    observe({
       if (is.DS(cdata)) {
           ws <<-getWS(cdata)
           getInit()
       } else {
          isolate({updateTabItems(session, "IdMenu", "about")})
          js$hideSidebar()
          js$hideSidebarToggle()
       }
    })

    # Display the Dataset Name 
    output$datasetname <- renderText({ ws[2] })

    # Session Info within the About tab
    output$sessioninfo <- renderPrint({
       print(sessionInfo())
    })

    #----------------------------------------------------
    # dynamically building of the UI of each analysis tab depending on the selected data subset
    # ( based on the onChange event, linked to the 'inDSelect' input - See the 'R/Plot_XXX.R' files )
    #----------------------------------------------------
    observe({
        updateSelectInput(session, "inDSelect", choices = DSL)
    })

})
