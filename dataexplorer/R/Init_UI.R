
    getURLfromList <- function() {
       collection <- as.data.frame(dclist$list)
       setID <- which( ws[2] == .C(collection$datasetID) )
       urls <- .C(collection$url)
       theurl <- externalURL
       if (setID>0 && !is.na(urls[setID]) && nchar(urls[setID])>0) { theurl <- urls[setID] }
       theurl
    }

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
    # Observer - Init
    #----------------------------------------------------
    observe({ tryCatch({
        if (is.DS(cdata)) {
           ws <<-getWS(cdata)
           if ( nchar(ws[5])>0 ) {
              dclist <<- getDataCol(ws)
              values$initcol <<- 1
           } else {
              inDselect <<- ws[2]
              js$hideinDselect()
              values$initds <<- 1
           }
        } else {
           isolate({updateTabItems(session, "IdMenu", "about")})
           js$hideSidebar()
           js$hideSidebarToggle()
           js$hideinDSselect()
        }
    }, error=function(e) { ERROR$MsgErrorMain <-  paste("Init Obs:\n", e, ", ws: ", paste( ws , collapse=" - ") ); }) })

    #----------------------------------------------------
    # Observer - Dataset list
    #----------------------------------------------------
    observe({ tryCatch({
        values$initcol
        if (values$initcol==1 && ! is.null(dclist)) {
            #listlabels <- dclist$list$description
            listlabels <- dclist$list$label
            indx <- order(listlabels)
            choices <- .C(dclist$list$datasetID[indx])
            names(choices) <- .C(listlabels[indx])
            inDselect <<- choices[1]
            values$init <<- 0
            values$initcol <<- 0
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
        values$initds
        if ( values$initds==1 || (! is.null(input$inDselect) && nchar(input$inDselect)>0 )) {
           if (! is.null(input$inDselect) && nchar(input$inDselect)>0 ) {
               ws[2] <<- input$inDselect
               ws[4] <<- getURLfromList()
               values$init <<- 0
           }
           tryCatch({ getInit() }, error=function(e) { ERROR$MsgErrorMain <- paste("getInit: ", e, ", ws: ", paste( ws , collapse=" - ") ); })
           inDSselect <<- 0
           # Default data subset
           if (! is.null(ws[6]) ) {
               N <- which(subsetNames==ws[6])
               if (length(N)>0 && N>0) {
                   inDSselect <<- N
                   getVars(N)
               }
           }
           if (! is.null(ws[7]) && ws[7] %in% c('datatable','univariate','bivariate','multivariate') ) {
              js$openTab(ws[7])
              js$hideSidebar()
              if (! is.null(ws[8]) && ws[8] %in% c('off') ) {
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

    observe({ tryCatch({
        if (! is.null(input$inDselect) && nchar(input$inDselect)>0 ) {
           if (input$inDselect != inDselect) {
              inDselect <<- input$inDselect
              values$launch <<- values$launch + 1
           }
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Data subset Change Obs:\n", e); }) })

    # Display the Collection/Dataset Name 
    output$datasetname <- renderText({ 
          if( nchar(ws[5])==0 ) {
             ws[2]
          } else {
             as.character(dclist$collection$Description)
          }
    })
