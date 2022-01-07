
    getURLfromList <- function() {
       collection <- as.data.frame(g$dclist$list)
       setID <- which( g$inDselect == .C(collection$datasetID) )
       urls <- .C(collection$url)
       theurl <- gv$externalURL
       if (setID>0 && !is.na(urls[setID]) && nchar(urls[setID])>0) { theurl <- urls[setID] }
       theurl
    }

    # Toogle (show/hide) analysis tabs
    analysisTab <- function(tabnames, opt)
    {
       for (tab in tabnames)
          if (opt==0) {
             addCssClass(selector = paste0("a[data-value='",tab,"']"), class = "inactiveItem")
          } else {
             removeCssClass(selector = paste0("a[data-value='",tab,"']"), class = "inactiveItem")
          }
    }

    # Toogle (show/hide) some UI elements
    toggleTab <- function(opt) {
       if (opt==0) { # About only
          isolate({updateTabItems(session, "IdMenu", "about")})
          runjs('$(".box-header").css("display", "none");')
          js$hideSidebar()
          js$hideSidebarToggle()
          js$hideinDselect()
          js$hideinDSselect()
       }
       if (opt==1) { # Collection only
          js$openTab("collection")
          js$showSidebar()
          js$showinDselect()
          js$hideinDSselect()
          removeCssClass(selector = "a[data-value='collection']", class = "inactiveItem")
          addCssClass(selector = "a[data-value='information']", class = "inactiveItem")
          addCssClass(selector = "a[data-value='intersection']", class = "inactiveItem")
          analysisTab(tabnames,0)
          runjs('$(".div-session").css("display", "none");')
       }
       if (opt==2) { # Dataset without Collection
          js$openTab("information")
          if (nchar(ui$tab)==0 || ! ui$tab %in% tabnames) js$showSidebar()
          js$hideinDselect()
          js$showinDSselect()
          addCssClass(selector = "a[data-value='collection']", class = "inactiveItem")
          removeCssClass(selector = "a[data-value='intersection']", class = "inactiveItem")
          analysisTab(tabnames,0)
          runjs('$(".div-session").css("display", "block");')
       }
       if (opt==3) { # Collection + Dataset
          js$openTab("information")
          if (nchar(ui$tab)==0 || ! ui$tab %in% tabnames) js$showSidebar()
          js$showinDselect()
          js$showinDSselect()
          removeCssClass(selector = "a[data-value='collection']", class = "inactiveItem")
          removeCssClass(selector = "a[data-value='information']", class = "inactiveItem")
          removeCssClass(selector = "a[data-value='intersection']", class = "inactiveItem")
          analysisTab(tabnames,0)
          runjs('$(".div-session").css("display", "block");')
       }
    }

    #----------------------------------------------------
    # Modal dialog UI for global parameters
    #----------------------------------------------------
    gparamsModal <- function() {
      modalDialog(
        tags$table(
          tags$tr(tags$td(tags$strong("Global parameters"))), tags$tr(tags$td(tags$hr())),
          tags$tr(tags$td(width="400px", tags$strong("Nb max variables for a datasubset so that it can be explored"))),
          tags$tr(tags$td(
              numericInput("maxVariables", NULL, gv$maxVariables, min = 100, max = 1000, step=100))),
          tags$tr(tags$td(width="400px",
              checkboxInput('subsetVars', 'Keep only the first variables limited to the maximum allowed in case of greater size', gv$subsetVars))),
          tags$tr(tags$td(
              numericInput("msmaxnbopt", "Nb max items when multiselect", gv$nbopt_multiselect, min = 10, max = 1000, step=50))),
          tags$tr(tags$td(checkboxInput("saveplots", "Save plots (GGM & COR)", gv$saveplots)))
        ),
        footer = tagList(
           modalButton("Cancel"), actionButton("okgparams", "Submit")
        )
      )
    }

    # When OK button is pressed, affects the global variables
    observeEvent(input$okgparams, {
        gv$maxVariables <<- input$maxVariables
        gv$subsetVars <<- input$subsetVars
        gv$nbopt_multiselect <<- input$msmaxnbopt
        gv$saveplots <<- input$saveplots
        if (gv$saveplots) {
          SESSTMPDIR <<- file.path(getwd(),'www/tmp',SESSID)
          dir.create(SESSTMPDIR, showWarnings = FALSE)
        } else {
          SESSTMPDIR <<- tempdir()
        }
        removeModal()
    })

    #----------------------------------------------------
    # Modal dialog UI for Packages information
    #----------------------------------------------------
    sessInfoModal <- function() {
      modalDialog({
          V <- sessionInfo();
          p <- ls(V$loadedOnly)
          packages <- NULL
          for (i in 1:length(p))
             packages <- c( packages, paste0(V$loadedOnly[[p[i]]]$Package,'_',V$loadedOnly[[p[i]]]$Version) )
          p <- ls(V$otherPkgs)
          others <- NULL
          for (i in 1:length(p))
             others <- c( others, paste0(V$otherPkgs[[p[i]]]$Package,'_',V$otherPkgs[[p[i]]]$Version) )
          tags$table(
             tags$tr(tags$td(colspan = 2, tags$strong(paste("Dataexplorer version",gv$idVersion)))),
             tags$tr(tags$td(colspan = 2, tags$hr())),
             tags$tr(tags$td(colspan = 2, V$R.version$version.string)),
             tags$tr(tags$td(colspan = 2, paste("Running under:",V$running))),
             tags$tr(tags$td(colspan = 2, paste("platform:",V$platform))),
             tags$tr(tags$td(colspan = 2, tags$br())),
             tags$tr(tags$td(style="vertical-align: top;", "Blas:"), tags$td(V$BLAS)),
             tags$tr(tags$td(style="vertical-align: top;", "Lapack:"), tags$td(V$LAPACK)),
             tags$tr(tags$td(colspan = 2, tags$br())),
             tags$tr(tags$td(style="vertical-align: top;", "locale:"), tags$td(gsub(';',', ', V$locale))),
             tags$tr(tags$td(colspan = 2, tags$br())),
             tags$tr(tags$td(style="vertical-align: top;", "Base Packages:"), tags$td(paste(V$basePkgs, collapse=', '))),
             tags$tr(tags$td(colspan = 2, tags$br())),
             tags$tr(tags$td(style="vertical-align: top;", "Loarded Packages:"), tags$td( paste(packages, collapse=', ') )),
             tags$tr(tags$td(colspan = 2, tags$br())),
             tags$tr(tags$td(style="vertical-align: top;", "Others Packages:"), tags$td( paste(others, collapse=', ') ))
          )
      },
      footer = modalButton("Dismiss"), size="l", easyClose = TRUE )
    }

    # When Ctrl-M is pressed, open the modal
    observeEvent(input$keyEvent, {
        if (input$keyEvent==9) showModal(sessInfoModal())
        if (input$keyEvent==10) showModal(gparamsModal())
        runjs('Shiny.onInputChange("keyEvent", 0);')
    })

    #----------------------------------------------------
    # Modal dialog UI for API Key
    #----------------------------------------------------
    apiKeyModal <- function(failed = FALSE) {
      addCssClass(selector = "a[data-value='intersection']", class = "inactiveItem")
      modalDialog(
        tags$table(tags$tr( 
           tags$td(passwordInput("authkey", "Enter the API Key", width = '400px', placeholder = '' )), 
           tags$td(tags$img(id='eyeapikey', src = "eye-close.png", width=30, onclick="javascript:eyetoggle();"))
        )),
        if (failed)
           div(tags$b("Invalid API Key", style = "color: red;")),
        footer = tagList(
           modalButton("Cancel"), actionButton("okApiKey", "Submit")
        )
      )
    }

    # When OK button is pressed, check if API Key is valid. 
    # If successful, remove the modal. 
    # If not show another modal, but this time with a failure message.
    observeEvent(input$okApiKey, {
      # Check if API Key is valid
      ws$auth <<- input$authkey
      ws$keymode <<- 2
      out <- getData(ws)
      if (is.wsError() && is.wsNoAuth()) {
         showModal(apiKeyModal(failed = TRUE))
      } else {
         removeModal()
         removeCssClass(selector = "a[data-value='intersection']", class = "inactiveItem")
         if ( nchar(ws$dsname)>0 ) { 
             values$initds <- values$initds + 1;
             values$init <- values$initdss <- values$launch <- values$error <- 0
         } else if ( nchar(ws$dcname)>0 ) { 
             values$initcol <- values$initcol + 1
         }
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
        if (values$init>0) runjs(paste0("uiloaded=1; ipclient='",input$ipclient,"'; apikey='",ws$auth,"';"))
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
    # Reactive - API Key is needed and has to be hidden
    #----------------------------------------------------
    output$hidekey <- reactive({
       values$init
       ret=0
       if (ws$keymode==2 && nchar(ws$auth)>0)
          ret=1
       return(ret)
    })
    outputOptions(output, 'hidekey', suspendWhenHidden=FALSE)
    outputOptions(output, 'hidekey', priority=1)

    #----------------------------------------------------
    # Reactive - API Error
    #----------------------------------------------------
    output$apierror <- reactive({
       values$init
       values$error
       ret=0
       if (values$error>0 && values$init>0) ret=1
       return(ret)
    })
    outputOptions(output, 'apierror', suspendWhenHidden=FALSE)
    outputOptions(output, 'apierror', priority=1)

    observe ({
       values$error
       if (values$error>0 && (values$initcol>0 || values$initds>0)) {
          js$hideinDSselect()
          js$showSidebar()
          if( nchar(ws$dcname)>0 ) { js$openTab("collection")  } 
          else                     { js$openTab("information") }
       }
    })


    #----------------------------------------------------
    # Reactive - No data set / data collection
    #----------------------------------------------------
    output$nods <- reactive({
       values$nods
       ret=0
       if (values$nods>0 && values$initds==0 && values$initcol==0) {
          ret=1
       }
       return(ret)
    })
    outputOptions(output, 'nods', suspendWhenHidden=FALSE)
    outputOptions(output, 'nods', priority=1)



    #----------------------------------------------------
    # Observer - Items selection - Show / Hide navbar selection
    #----------------------------------------------------
    observeEvent(input$IdMenu, { 
       if(input$IdMenu=="about"){
          js$hideinDselect()
          js$hideinDSselect()
       }
       else if(input$IdMenu=="collection"){
          js$showinDselect()
          js$hideinDSselect()
       }
       else {
          if (! is.null(g$dclist)) js$showinDselect()
          js$showinDSselect()
       }
    })


    #----------------------------------------------------
    # Observer - Init: Parse URL parameters
    #----------------------------------------------------
    observe({ tryCatch({
        input$ipclient
        if (nchar(input$ipclient)>0) {
           if (is.DS(cdata)) { # query string ...
              getURLparams(cdata)
              ws$ipclient <<- input$ipclient
              if ( nchar(ws$dcname)>0 ) { # ... with a collection
                 g$dclist <<- getDataCol(ws)
                 if (is.wsError()) {
                     if (is.wsNoAuth()) { showModal(apiKeyModal()) }
                     else               { values$init <- values$error <- 1 }
                 } else {
                     values$initcol <- 1
                 }
                 toggleTab(1)
              } else { # ... with a dataset
                 g$inDselect <<- ws$dsname
                 toggleTab(2)
                 values$initds <- 1
              }
           } else { # no query string
              toggleTab(0)
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
        if (nchar(input$ipclient)>0 && values$initcol==1 && ! is.null(g$dclist)) {
            listlabels <- g$dclist$list$label
            indx <- order(listlabels)
            choices <- .C(g$dclist$list$datasetID[indx])
            names(choices) <- .C(listlabels[indx])
            g$inDselect <<- ''
            values$init <- 1
            updateSelectInput(session, "inDselect", choices = c("Select a dataset"="",choices), selected = g$inDselect)
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Collection Obs:\n", e ); }) })

    # Display the Collection/Dataset Name 
    output$datasetname <- renderText({
        values$initds
        values$initcol
        if( nchar(ws$dcname)==0 ) {
           ws$dsname
        } else {
           as.character(g$dclist$collection$Description)
        }
    })


    #----------------------------------------------------
    # Observer - if dataset change
    #----------------------------------------------------
    observe({ tryCatch({
        input$inDselect
        values$initds
        if (! is.null(input$inDselect) && nchar(input$inDselect)>0 && g$inDselect != input$inDselect) {
           g$inDselect <<- input$inDselect
        }
        if ( ! is.null(g$inDselect) && nchar(g$inDselect)>0 ) {
           if (ws$dsname != g$inDselect) {
              ws$dsname <<- g$inDselect
              listlabels <- g$dclist$list$label
              indx <- order(listlabels)
              choices <- .C(g$dclist$list$datasetID[indx])
              names(choices) <- .C(listlabels[indx])
              updateSelectInput(session, "inDselect", choices = c("Select a dataset"="",choices), selected = g$inDselect)
              runjs('window.scrollTo(0,0);')
           }
           toggleTab(2)
           if (nchar(ws$dcname)>0) {
              ws$apiurl <<- getURLfromList()
              values$initds <- which( .C(g$dclist$list$datasetID[order(g$dclist$list$label)]) == g$inDselect )
              toggleTab(3)
           }
           values$initdss <- values$initds
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Dataet Change Obs:\n", e); }) })


    #----------------------------------------------------
    # Observer - Dataset subset init
    #----------------------------------------------------
    observe({ tryCatch({
        values$initdss
        if ( nchar(input$ipclient)>0 && values$initdss>0 ) {
           tryCatch({ getInit() }, error=function(e) { ERROR$MsgErrorMain <- paste("getInit: ", e, ", ws: ", paste( ws , collapse=" - ") ); })
           if (is.wsError()) {
               if (is.wsNoAuth()) { showModal(apiKeyModal()) }
               else               { values$init <- values$error <- 1 }
           }
           g$inDSselect <<- ''
           DSselect <- NULL
           # Default data subset
           if (! is.null(ws$subset) && ! is.na(ws$subset) && nchar(ws$subset)>0 ) {
               DSselect <- .S(ws$subset)
               if (! is.varsExist(DSselect)) { values$init <- values$error <- 1 }
           }
           if ( ui$tab %in% tabnames ) {
              js$openTab(ui$tab)
              js$hideSidebar()
              if (ui$header %in% c('off')) {
                  js$hideMainHeader()
                  runjs('$(".box-header").css("display", "none");')
                  runjs('$(".skin-blue, .sidebar-collapse").css("min-height", "0px");')
                  if ( ui$updiv %in% c('off') )
                       runjs('$(".div-top").css("display", "none");')
                  if ( ui$downdiv %in% c('off') )
                       runjs(paste('$(".div-down").css("display", "none");',
                                '$(".content-wrapper").css("min-height","0px");'))
              }
           }
           if ( nchar(ui$tab)>0 && ! ui$tab %in% tabnames ) {
              g$msgError <<- paste0("ERROR: '",ui$tab,"' is not a valid tab name")
              values$error <- 1; values$initdss <- 0
           }
           values$init <- 1

           #----------------------------------------------------
           # dynamically building of the UI of each analysis tab depending on the selected data subset
           # ( based on the onChange event, linked to the 'inDSselect' input - See the 'R/Plot_XXX.R' files )
           #----------------------------------------------------
           if (! is.null(g$DSL) && ! is.wsError()) {
              updateSelectInput(session, "inDSselect", label=NULL, choices = c("Select one or more data subset"="", g$DSL), selected = DSselect )
           }
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Data subset Obs:\n", e, ':', paste(g$DSL, collapse=", ")); }) })


    #----------------------------------------------------
    # Observer - if data subset change
    #----------------------------------------------------
    observe({ tryCatch({
        input$inDSselect
        if (! is.null(g$subsets) && (is.null(input$inDSselect) || length(input$inDSselect)==0)) {
             g$inDSselect <<- ws$subset <<- ''; values$initds <- values$initdss <- values$launch <- 0
        }
        if (! is.null(g$subsets) && ! is.null(input$inDSselect) && length(input$inDSselect)>0 && g$inDSselect != .J(input$inDSselect)) {
            getVars(.J(input$inDSselect))
            if (is.wsError()) { values$init <- values$error <- 1; values$initdss <- 0; values$launch <- 0; analysisTab(tabnames,0) }
            else              { values$launch <- length(input$inDSselect); analysisTab(tabnames,1) }
        }
    }, error=function(e) { ERROR$MsgErrorMain <- paste("Data subset Change Obs:\n", e); }) })


    #----------------------------------------------------
    # Reactive - length of the input Data Subsets inDSselect
    #----------------------------------------------------
    output$DSsize <- reactive({
       values$launch
       ret <- length(input$inDSselect)
       return(ret)
    })
    outputOptions(output, 'DSsize', suspendWhenHidden=FALSE)
    outputOptions(output, 'DSsize', priority=1)

    #----------------------------------------------------
    # Reactive - length of the input Data Subsets inDSselect
    #----------------------------------------------------
    output$DSsize <- reactive({
       values$launch
       ret <- length(input$inDSselect)
       return(ret)
    })
    outputOptions(output, 'DSsize', suspendWhenHidden=FALSE)
    outputOptions(output, 'DSsize', priority=1)


    #----------------------------------------------------
    # Reactive - Dataset subset varnames 
    #----------------------------------------------------
    output$nbvarsEvent <- reactive({
        input$inDselect
        values$launch
        ret <- 0
        if (values$launch>0) {
           if (nrow(g$varnames)>gv$maxVariables) ret <- 1
           if (nrow(g$varnames)<3) {
               analysisTab(tabnames,0); 
               if (nrow(g$varnames)<2)
                  analysisTab(subtabnames2,1)
               else
                  analysisTab(subtabnames,1)
           }
        }
        return(ret)
    })
    outputOptions(output, 'nbvarsEvent', suspendWhenHidden=FALSE)
    outputOptions(output, 'nbvarsEvent', priority=1)

    output$nbvarsEvent2 <- reactive({
        input$inDselect
        values$launch
        ret <- 0
        if (values$launch>0) {
           if (nrow(g$varnames)>gv$max_multivars) ret <- 1
        }
        return(ret)
    })
    outputOptions(output, 'nbvarsEvent2', suspendWhenHidden=FALSE)
    outputOptions(output, 'nbvarsEvent2', priority=1)
