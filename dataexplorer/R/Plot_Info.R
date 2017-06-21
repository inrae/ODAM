    #----------------------------------------------------
    # Observer - Data Table
    #----------------------------------------------------
    observe({
       if ( ! is.null(input$inDSelect) && input$inDSelect>0) {
          if (inDSelect != input$inDSelect) getVars(.N(input$inDSelect))
          fa_options <- colnames(data)
          names(fa_options) <- colnames(data)
          updateCheckboxGroupInput(session, 'show_vars', label = 'Columns to show:', choices = fa_options,  
                selected = c( samples, .C(facnames$Attribute) ))
        }
    })

    #----------------------------------------------------
    # Session Info within the About tab
    #----------------------------------------------------
    output$sessioninfo <- renderPrint({
       if (nchar(ws[2])==0) {
         print(sessionInfo())
       } else {
        # Run JS code  
         cat("options(width=256)\n", "options(warn=-1)\n","options(stringsAsFactors=FALSE)\n","\n",
             "library(Rodam)\n","\n",
             "# Initialize the 'ODAM' object \n", "dh <- new('odamws', '",ws[4],"', '",ws[2],"')\n","\n",
             "# Get the Data Tree\n","show(dh)\n","\n",
             "# Get the data subsets list\n","dh$subsetNames\n","\n",
             sep=""
         )
         if ( input$inDSelect>0) {
            if (inDSelect != input$inDSelect) getVars(.N(input$inDSelect))
            setName <- subsetNames[inDSelect]
            cat(
               "# Get '",setName,"' data subset\n", "ds <- dh$getSubsetByName('",setName,"')\n", "\n",
               "# Show all descriptions of variables\n", "ds$LABELS\n","\n",
               "# Show all factors defined in the data subset\n","ds$facnames\n","\n",
               "# Show all quantitative variables defined in the data subset\n","ds$varnames\n","\n",
               "# Show all qualitative variables defined in the data subset\n","ds$qualnames\n","\n",
               "# Display a summary for each quantitative variable\n","summary(ds$data[, ds$varnames ])\n","\n",
               "# Boxplot of all variables defined in ds$varnames\n",
               "Rank <- simplify2array(lapply(ds$varnames, function(x) { round(mean(log10(ds$data[ , x]), na.rm=T)) }))\n",
               "colRank <- Rank - min(Rank) + 1\n",
               "cols <- c('red', 'orange', 'darkgreen', 'blue', 'purple', 'brown')\n",
               "boxplot(log10(ds$data[, ds$varnames]), outline=F, horizontal=T, border=cols[colRank], las=2, cex.axis=0.5)\n","\n",
               sep=""
            )
         }
       }
    })

    #----------------------------------------------------
    # renderUI - Information
    #----------------------------------------------------
    output$subsets <- renderDataTable({ 
        if ( !is.DS(cdata) || values$init==0 ||  is.null(input$inDSelect)) return(NULL)
        if (input$inDSelect>0) {
            tsets <- subsets[subsets$Subset==subsetNames[.N(input$inDSelect)], ]
        } else {
            tsets <- subsets
        }
        setinfo <- NULL
        for( i in 1:dim(tsets)[1]) {
            urlSubset <- paste0(ws[4],'xml/', ws[2], '/(',.C(tsets[i,1]) ,")?auth=",ws[3]);
            linkSubset <- paste0("<a href='",urlSubset,"' target='_blank'>",.C(tsets[i,1]),"</a>")
            linkOnto <- paste0("<a href='",.C(tsets[i,5]),"' target='_blank'>[", basename(.C(tsets[i,5])),'] ', .C(tsets[i,6]),"</a>")
            setinfo <- rbind( setinfo , c( linkSubset, .C(tsets[i,c(2:4)]), linkOnto ) )
        }
        df <- as.data.frame(setinfo)
        names(df) <- c("Subset","Description","Identifier", "WSEntry", "CV_Term")
        df

    }, options = list(searching=FALSE, paging=FALSE), escape=c(2:4))

    output$infos <- renderDataTable({
        if (is.null(input$inDSelect) || input$inDSelect==0) return(NULL)
        if (is.null(LABELS) || dim(LABELS)[1]==0) return(NULL)
        getLabels()
    }, options = list(searching=FALSE, paging=FALSE), escape=c(1,2,3))

    output$wait <- renderUI({
        values$init
        if (values$init==1) { "" }
    })

    #----------------------------------------------------
    # Export the selected data subset 
    #----------------------------------------------------
    output$downloadTSV <- downloadHandler(
        filename = function() {
            paste('data_', .C(subsetNames[.N(input$inDSelect)]),'_', Sys.Date(), '.tsv', sep='')
        },
        content = function(con) {
            setName <- .C(subsets[ subsets$Subset==subsetNames[.N(input$inDSelect)], ][1,1])
            write.table(data, con, sep="\t", row.names=FALSE, col.names=TRUE)
        }
    )

    #----------------------------------------------------
    # renderUI - Subsets Graph
    #----------------------------------------------------
    output$Net <- renderDiagonalNetwork({
        if (values$init==0) values$init <- 1
        if (length(subsetNames)>0) {
             diagonalNetwork(List = dn, fontSize = fs, fontFamily = "serif", 
                 linkColour = '#B2B3D0', nodeColour = "#fff", nodeStroke = "red", textColour = "darkblue",
                 opacity = 0.99)
        }
    })

    #----------------------------------------------------
    # renderUI - Data Table - See http://rstudio.github.io/DT/
    #----------------------------------------------------
    output$datavalues <- DT::renderDataTable({
       DT::datatable( data[, input$show_vars, drop = FALSE],  selection='none', filter = 'top', rownames = FALSE, options= list( pageLength = 25, autoWidth=TRUE ) )
       #DT::datatable(data[, input$show_vars, drop = FALSE], selection='none', extensions = c('Buttons','Scroller'), filter = 'top', options= list(
       #     dom='Brtp', buttons = list('copy','excel'), pageLength = dim(data)[1], autoWidth=TRUE,  deferRender = FALSE,  scrollY = 750,  scroller = TRUE
       #))
    #}, server=FALSE )
    })

    output$datavalues <- DT::renderDataTable(
       data[, input$show_vars, drop = FALSE],  selection='none', filter = 'top', rownames = FALSE,  
       #options= list( pageLength = 25, autoWidth=TRUE ), server=TRUE
       extensions = c('Buttons','Scroller'), options= list(
              dom='Bfrtip', buttons = list('copy','excel'), pageLength = dim(data)[1], autoWidth=TRUE,  deferRender = FALSE,  scrollY = 750,  scroller = TRUE
       ), server=FALSE
    )