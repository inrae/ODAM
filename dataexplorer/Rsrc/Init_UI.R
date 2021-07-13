
    getURLfromList <- function() {
       collection <- as.data.frame(dclist$list)
       setID <- which( ws[2] == .C(collection$datasetID) )
       urls <- .C(collection$url)
       theurl <- externalURL
       if (setID>0 && !is.na(urls[setID]) && nchar(urls[setID])>0) { theurl <- urls[setID] }
       theurl
    }

    #----------------------------------------------------
    # Modal dialog UI
    #----------------------------------------------------
    apiKeyModal <- function(failed = FALSE) {
      modalDialog(
        passwordInput("authkey", "Enter the API Key", width = '400px', placeholder = '' ),
        if (failed)
           div(tags$b("Invalid API Key", style = "color: red;")),
        footer = tagList(
           modalButton("Cancel"), actionButton("okApiKey", "OK")
        )
      )
    }

    # When OK button is pressed, check if API Key is valid. 
    # If successful, remove the modal. 
    # If not show another modal, but this time with a failure message.
    observeEvent(input$okApiKey, {
      # Check if API Key is valid
      ws[3] <<- input$authkey
      ws[1] <<- 2
      out <- getData(ws)
      if (is.wsError() && is.wsNoAuth()) {
         showModal(apiKeyModal(failed = TRUE))
      } else {
         removeModal()
         if ( nchar(ws[5])>0 ) { values$initcol <- values$initcol + 1 }
         else                  { values$initds <- values$initds + 1 }
         values$error <- 0
      }
    })

    #----------------------------------------------------
    # Observer - loading is complete
    #----------------------------------------------------
    observe ({
        values$init
        values$initds
        values$initcol
        if (values$init>0) runjs(paste0("uiloaded=1; ipclient='",input$ipclient,"'; apikey='",ws[3],"';"))
    })

    #----------------------------------------------------
    # Observer - ERROR
    #----------------------------------------------------
    observe ({
       ERROR$MsgErrorMain
       if (nchar(ERROR$MsgErrorMain)>0) {
          createAlert(session, "ErrAlertMain", "ErrAlertMainId", title = "", content = ERROR$MsgErrorMain, append = FALSE, style='danger')
       }
    })

    #----------------------------------------------------
    # Reactive - API Error
    #----------------------------------------------------
    output$apierror <- reactive({
       values$error
       ret=0
       if (values$error>0 && (values$initds>0 || values$initcol>0)) {
          #js$hideSidebar()
          js$hideinDSselect()
          ret=1
       } else {
          js$showSidebar()
          js$showinDSselect()
       }
       ret
    })
    outputOptions(output, 'apierror', suspendWhenHidden=FALSE)
    outputOptions(output, 'apierror', priority=1)

    #----------------------------------------------------
    # Reactive - No data set / data collection
    #----------------------------------------------------
    output$nods <- reactive({
       values$nods
       ret=0
       if (values$nods>0 && values$initds==0 && values$initcol==0) {
          ret=1
       }
       ret
    })
    outputOptions(output, 'nods', suspendWhenHidden=FALSE)
    outputOptions(output, 'nods', priority=1)

    #----------------------------------------------------
    # Observer - Init
    #----------------------------------------------------

    observe({ tryCatch({
        input$ipclient
        if (nchar(input$ipclient)>0) {
           if (is.DS(cdata)) {
              ws <<-getWS(cdata)
              ws[6] <<- input$ipclient
              if ( nchar(ws[5])>0 ) {
                 dclist <<- getDataCol(ws)
                 if (is.wsError()) {
                     if (is.wsNoAuth()) showModal(apiKeyModal())
                     values$error <- 1
                 } else {
                     values$initcol <- 1
                 }
              } else {
                 inDselect <<- ws[2]
                 js$hideinDselect()
                 values$initds <- 1
              }
           } else {
              isolate({updateTabItems(session, "IdMenu", "about")})
              js$hideSidebar()
              js$hideSidebarToggle()
              js$hideinDselect()
              js$hideinDSselect()
              values$nods <- 1
           }
        }
    }, error=function(e) { ERROR$MsgErrorMain <-  paste("Init Obs:\n", e, ", ws: ", paste( ws , collapse=" - ") ); }) })

    #----------------------------------------------------
    # Observer - Dataset list
    #----------------------------------------------------
    observe({ tryCatch({
        input$ipclient
        values$initcol
        if (nchar(input$ipclient)>0 && values$initcol==1 && ! is.null(dclist)) {
            listlabels <- dclist$list$label
            indx <- order(listlabels)
            choices <- .C(dclist$list$datasetID[indx])
            names(choices) <- .C(listlabels[indx])
            inDselect <<- choices[1]
            values$init <- 0
            values$initcol <- 0
            if (! is.null(ws[2]) && nchar(ws[2])>0 )
               inDselect <<- ws[2]
            else
               ws[2] <<- inDselect
            ws[4] <<- getURLfromList()
            updateSelectInput(session, "inDselect", choices = choices, selected = inDselect)
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Collection Obs:\n", e ); }) })

    #----------------------------------------------------
    # Observer - Dataset subset init
    #----------------------------------------------------
    observe({ tryCatch({
        input$ipclient
        values$initds
        if ( nchar(input$ipclient)>0 && 
            ((values$initds>0) || (! is.null(input$inDselect) && nchar(input$inDselect)>0)) ) {
           if (! is.null(input$inDselect) && nchar(input$inDselect)>0 ) {
               ws[2] <<- input$inDselect
               ws[4] <<- getURLfromList()
               values$init <- 0
           }
           tryCatch({ getInit() }, error=function(e) { ERROR$MsgErrorMain <- paste("getInit: ", e, ", ws: ", paste( ws , collapse=" - ") ); })
           if (is.wsError()) {
               if (is.wsNoAuth()) showModal(apiKeyModal())
               values$error <- 1
           }
           inDSselect <<- 0
           # Default data subset
           if (! is.null(ws[7]) ) {
               N <- which(subsetNames==ws[7])
               if (length(N)>0 && N>0) {
                   inDSselect <<- N
                   getVars(N)
               }
           }
           if (! is.null(ws[8]) && ws[8] %in% c('datatable','univariate','bivariate','multivariate') ) {
              js$openTab(ws[8])
              js$hideSidebar()
              if (! is.null(ws[9]) && ws[8] %in% c('off') ) {
                  js$hideMainHeader()
              }
           }
           #----------------------------------------------------
           # dynamically building of the UI of each analysis tab depending on the selected data subset
           # ( based on the onChange event, linked to the 'inDSselect' input - See the 'R/Plot_XXX.R' files )
           #----------------------------------------------------
           if (! is.null(DSL)) {
              updateSelectInput(session, "inDSselect", choices = DSL, selected = inDSselect)
           }
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Data subset Obs:\n", e); }) })

    #----------------------------------------------------
    # Observer - if dataset change
    #----------------------------------------------------
    observe({ tryCatch({
        if (! is.null(input$inDselect) && nchar(input$inDselect)>0 ) {
           if (input$inDselect != inDselect) {
              inDselect <<- input$inDselect
              values$launch <- values$launch + 1
           }
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Data subset Change Obs:\n", e); }) })

    # Display the Collection/Dataset Name 
    output$datasetname <- renderText({
        values$initds
        values$initcol
        if( nchar(ws[5])==0 ) {
           ws[2]
        } else {
           as.character(dclist$collection$Description)
        }
    })

    #----------------------------------------------------
    # Reactive - Dataset subset varnames 
    #----------------------------------------------------
    output$nbvarsEvent <- reactive({
        input$inDselect
        ret <- 0
        if (! is.null(input$inDSselect) && input$inDSselect>0) {
           if (inDSselect != input$inDSselect) getVars(.N(input$inDSselect))
           if (dim(varnames)[1]>maxVariables) ret <- 1
        }
        return(ret)
    })
    outputOptions(output, 'nbvarsEvent', suspendWhenHidden=FALSE)
    outputOptions(output, 'nbvarsEvent', priority=1)

