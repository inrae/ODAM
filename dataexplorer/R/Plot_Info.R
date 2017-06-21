
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
