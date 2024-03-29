    #----------------------------------------------------
    # Observer - ERROR
    #----------------------------------------------------
    observe ({
       ERROR$MsgErrorInfo
       if (nchar(ERROR$MsgErrorInfo)>0) {
          createAlert(session, "ErrAlertInfo", "ErrAlertInfoId", title = "", content = ERROR$MsgErrorInfo, append = FALSE, style='danger')
       }
    })
    observe ({
       ERROR$MsgErrorAbout
       if (nchar(ERROR$MsgErrorAbout)>0) {
          createAlert(session, "ErrAlertAbout", "ErrAlertAboutId", title = "", content = ERROR$MsgErrorAbout, append = FALSE, style='danger')
       }
    })

    #----------------------------------------------------
    # Observer - Intersection
    #----------------------------------------------------
    observe({ tryCatch({
       values$initdss
       values$init
       if (nchar(input$ipclient)==0) return(NULL)
       l_options <- c('0')
       names(l_options) <- c('---')
       if (length(g$subsetNames)>0) {
          setNameList <- c()
          for (id in unique(g$subsets$Identifier))
              setNameList <- c(setNameList,g$subsets$Subset[ g$subsets$SetID==min(g$subsets$SetID[ g$subsets$Identifier == id ])])
          l_options <- c('0', setNameList)
          names(l_options) <- c('---', g$subsets$Description[ g$subsets$Subset %in% setNameList ])
       }
       updateSelectInput(session, "interSubset", choices = l_options, selected='0')
    }, error=function(e) { ERROR$MsgErrorInfo <- paste("Observer 1:\n", e ); }) })

    #----------------------------------------------------
    # Observer - Data Table
    #----------------------------------------------------
    observe({ tryCatch({
       values$initdss
       values$launch
       if ( values$launch>0) {
          fa_options <- colnames(g$data)
          names(fa_options) <- colnames(g$data)
          updateCheckboxGroupInput(session, 'show_vars', label = 'Columns to show:', choices = fa_options,  
                selected = c( g$samples, .C(g$facnames$Attribute) ))
       }
    }, error=function(e) { ERROR$MsgErrorInfo <- paste("Observer 1:\n", e ); }) })

    #----------------------------------------------------
    # renderUI - Data Table - See http://rstudio.github.io/DT/
    #----------------------------------------------------
    output$datavalues <- tryCatch({
       DT::renderDataTable(
           unique( g$data[, input$show_vars, drop = FALSE] ), selection='none', filter = 'top', rownames = FALSE, 
           extensions = c('Buttons','Scroller'), 
           callback = JS( c("$('table.dataTable thead th').css('border-bottom', 'none');",
                          "$('table.dataTable.no-footer').css('border-bottom', 'none');") ),
           options = list(
                 dom='Bfrtip', buttons = list('copy','excel'), pageLength = nrow(g$data), autoWidth=TRUE,  
                 deferRender = FALSE,  scrollY = 750,  scroller = TRUE
           ), server=FALSE
       )
    }, error=function(e) { ERROR$MsgErrorInfo <- paste("DT::renderDataTable:\n", e ); })



    #----------------------------------------------------
    # renderUI - About Box
    #----------------------------------------------------
    output$aboutinfos <- renderText({
       tryCatch({
          getAboutToHTML()
       }, error=function(e) { ERROR$MsgErrorAbout <- paste("RenderText - About:\n", e ); })
    })



    #----------------------------------------------------
    # renderUI - Debug
    #----------------------------------------------------
    output$out1 <- renderText({ 
       input$keyEvent
       tryCatch({
          paste(
            "protocol: ", cdata$url_protocol, "\n",
            "hostname: ", cdata$url_hostname, "\n",
            "pathname: ", cdata$url_pathname, "\n",
            "port: ",     cdata$url_port,     "\n",
            "search: ",   cdata$url_search,   "\n",
            "key press: ",input$keyEvent,     "\n",
             sep = ""
          )
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderText - out1 \n", e ); })
    })



    #----------------------------------------------------
    # renderUI - Data Collection information
    #----------------------------------------------------
    output$colinfos <- renderText({
       values$initcol
       if (nchar(input$ipclient)==0) return(NULL)
       if (nchar(ws$dcname)==0) return(NULL)
       if (nchar(ws$dcname)==1) return(NULL)
       tryCatch({
          getInfosToHTML(ws,1)
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderText - Collection info \n", e ); })
    })

    #----------------------------------------------------
    # renderUI - DataTable of Data Collection
    #----------------------------------------------------
    output$datasets <- renderDataTable({
       values$initcol
       values$init
       if (nchar(input$ipclient)==0) return(NULL)
       if (nchar(ws$dcname)==0) return(NULL)
       if (nchar(ws$dcname)==1) return(NULL)
       tryCatch({ if (nchar(g$msgError)==0) {
          ws0 <- ws
          collect <- as.data.frame(g$dclist$list)
          # dataset
          V1 <- .C(collect[ .C(collect$datatype) == 'dataset', ]$datasetID)
          collect[ .C(collect$datatype) == 'dataset', ]$datasetID <- sapply(V1, function(ds) { 
                   paste0("<a onclick=\"Shiny.onInputChange('inDselect','",ds,"');\">",ds,"</a>") })
          collect[ .C(collect$datatype) == 'dataset', ]$url <- sapply( V1, function(ds) { ws0$dsname <- ds; nrow(getData(ws0)) })
          # collection
          V2 <- .C(collect[ .C(collect$datatype) == 'collection', ]$datasetID)
          collect[ .C(collect$datatype) == 'collection', ]$datasetID <- sapply(V2, function(dc) { 
                   paste0("<a href=\"?dc=",dc,"\" target=\"_blank\">",dc,"</a>") })
          collect[ .C(collect$datatype) == 'collection', ]$url <- rep( '-',length(V2))

          names(collect) <- c("Datatype", "Dataset", "Label", "Data subsets", "Description")
          collect
       }}, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderDataTable - datasets \n", e ); })
    }, options = list(searching=TRUE, paging=TRUE, autoWidth = TRUE), escape=c(1,3:5))



    #----------------------------------------------------
    # renderUI - Dataset Information
    #----------------------------------------------------
    output$datainfos <- renderText({
       values$initds
       values$error
       input$inDselect
       if (nchar(input$ipclient)==0) return(NULL)
       tryCatch({
          getInfosToHTML(ws,0)
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderText - Data info \n", e ); })
    })

    #----------------------------------------------------
    # renderUI - Collection Metadata
    #----------------------------------------------------
    output$colmetadata <- renderDataTable({
       values$initdss
       if (nchar(input$ipclient)==0) return(NULL)
       tryCatch({ if (nchar(g$msgError)==0) {
           if ( !is.DS(cdata) || values$init==0) return(NULL)
           getMetadataLinksAsTable2(ws)
       }}, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderDataTable - Collection Metadata \n", e ); })
    }, options = list(searching=FALSE, paging=FALSE, lengthChange = FALSE, info = FALSE), escape=c(2))



    #----------------------------------------------------
    # renderUI - Metadata of Data subsets
    #----------------------------------------------------
    output$metadata <- renderDataTable({
       values$initdss
       if (nchar(input$ipclient)==0) return(NULL)
       tryCatch({ if (nchar(g$msgError)==0) {
           if ( !is.DS(cdata) || values$init==0) return(NULL)
           if ( is.null(g$subsets2) ) return(NULL)
           getMetadataLinksAsTable(ws)
       }}, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderDataTable - Metadata \n", e ); })
    }, options = list(searching=FALSE, paging=FALSE, lengthChange = FALSE, info = FALSE), escape=c(2))



    #----------------------------------------------------
    # renderUI - DataTable of Data subsets
    #----------------------------------------------------
    output$subsets <- renderDataTable({
       values$initdss
       shinyjs::hide("downloadTSV")
       if (nchar(input$ipclient)==0) return(NULL)
       tryCatch({ if (nchar(g$msgError)==0) {
           if ( !is.DS(cdata) || values$init==0) return(NULL)
           if ( is.null(g$subsets2) ) return(NULL)
           tsets <- g$subsets2
           setinfo <- NULL
           authstr <- ifelse( ws$keymode<2 && nchar(ws$auth)>0, paste0('auth=', ws$auth,'&'), '' );
           runjs('arrDS=Array(0);')
           for( i in 1:nrow(tsets)) {
               ds <- .C(tsets[i,1])
               urlSubset <- paste0(ws$apiurl,'query/', ws$dsname, '/(',ds,')?', authstr, 'format=xml');
               linkSubset <- ifelse( ws$keymode<2 && gv$VPN==0,
                   paste0("<a href=\"",urlSubset,"\" target=\"_blank\">",ds,"</a>"),
                   paste0("<a href=\"javascript:void(0)\" onclick=\"openXML('",urlSubset,"');\">",ds,"</a>") )
               linkOnto <- paste0("<a href='",.C(tsets[i,5]),"' target='_blank'>[", basename(.C(tsets[i,5])),'] ', .C(tsets[i,6]),"</a>") 
               onclickStr <- paste0('if ($(\'#check_',i,'\').is(\':checked\')){arrDS[arrDS.length]=\'',ds,'\'; arrDS=arrDS.filter(function(e){return e});}
                                     else{arrDS=arrDS.filter(function(e){return e !== \'',ds,'\'});};
                                     if(arrDS.length){Shiny.onInputChange(\'dwnld_button\', arrDS.join(\',\'));}
                                     else{Shiny.onInputChange(\'dwnld_button\',\'\');}')
               checkDwn <- paste0('<input type="checkbox" id="check_',i,'" onclick="',onclickStr,'">')
               setinfo <- rbind( setinfo , c( linkSubset, .C(tsets[i,c(2:4)]), linkOnto, checkDwn ) )
           }
           shinyjs::show("downloadTSV")
           shinyjs::disable("downloadTSV")
           df <- as.data.frame(setinfo)
           names(df) <- c("Subset","Description","Identifier", "WSEntry", "CV_Term", "Export in TSV")
           df
       }}, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderDataTable - Subsets \n", e ); })
    }, options = list(searching=FALSE, paging=FALSE, lengthChange = FALSE, info = FALSE), escape=c(2:4))

    observeEvent ( input$dwnld_button, {
       if (nchar(input$dwnld_button)>0) { shinyjs::enable("downloadTSV") } else { shinyjs::disable("downloadTSV") }
    })

    #----------------------------------------------------
    # Export the selected Data subsets
    #----------------------------------------------------
    output$downloadTSV <- downloadHandler(
        filename = function() {
            inDSselect <- .S(input$dwnld_button)
            if (length(inDSselect)>1) {
               paste('data_', paste(g$subsets$SetID[g$subsetNames %in% inDSselect], collapse='-'), '_', Sys.Date(), '.tsv', sep='')
            } else {
               paste('data_', .C(g$subsetNames[g$subsetNames %in% inDSselect]),'_', Sys.Date(), '.tsv', sep='')
            }
        },
        content = function(con) {
            runjs('$(\'div[name="downldButton"]\').css(\'display\',\'inline-block\')');
            # Get data subsets based on selection and write to console
            tryCatch({
               strNameList <- .J(g$subsets[ g$subsetNames %in% .S(input$dwnld_button), ]$Subset)
               data <- getData(ws,paste('(',strNameList,')',sep=''))
               if(is.wsNoData())
                  showModal(messageModal("Intersection of the data subsets seems empty"))
               write.table(data, con, sep="\t", row.names=FALSE, col.names=TRUE)
            }, error=function(e) { ERROR$MsgErrorInfo <- paste("Download:", e ); })
            runjs('$(\'div[name="downldButton"]\').css(\'display\',\'none\')');
            # reset selection
            for( i in 1:nrow(g$subsets2)) runjs(paste0('$(\'#check_',i,'\').prop(\'checked\',false)'))
            shinyjs::disable("downloadTSV")
            runjs("arrDS=Array(0);")
        }
    )


    #----------------------------------------------------
    # renderUI - Metadata of the selected Data subsets
    #----------------------------------------------------
    output$infos <- renderDataTable({
       values$initdss
       tryCatch({ 
           if (values$launch==0) return(NULL)
           if (is.null(g$LABELS) || nrow(g$LABELS)==0) return(NULL)
           getLabels()
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderDataTable - Infos:\n", e ); })
    }, options = list(searching=FALSE, paging=FALSE, lengthChange = FALSE, info = FALSE), escape=c(1,2,3))


    #----------------------------------------------------
    # renderImage - Intersection
    #----------------------------------------------------
    # https://jokergoo.github.io/2020/05/15/interactive-complexheatmap/
    # https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html
    # https://www.quantargo.com/help/r/latest/packages/ComplexHeatmap/2.6.2/UpSet
    # 
    output$UpSetPlot <- renderPlot({
       values$initdss
       input$interSubset
       tryCatch({
           setIDS <- c( g$subsets$SetID[ g$subsets$Subset == .C(input$interSubset) ] )
           for (k in 1:nrow(g$subsets))
              if (g$subsets$LinkID[k] %in% setIDS) setIDS <- c( setIDS,g$subsets$SetID[k] )
           setNameList <- g$subsets$Subset[g$subsets$SetID %in% setIDS]
           refID <- g$subsets$Identifier[ g$subsets$Subset == .C(input$interSubset) ]

           setsList <- list()
           for( i in 1:length(setNameList)) {
               L <- getData(ws,paste(setNameList[i],'/quantitative', sep=''))
               if (nrow(L)>0) {
                   data <- getData(ws, paste('(',setNameList[i],')', sep=''))
                   setsList[[ setNameList[i] ]] <- unique(sort(data[ , refID ]))
               }
           }

           M = make_comb_mat(setsList, mode = "distinct")
           draw(UpSet(M, pt_size = unit(5, "mm"), lwd = 3,
                     comb_col = rev(brewer.pal(n = 10, name = "Dark2"))[comb_degree(M)], 
                          top_annotation = upset_top_annotation(M, 
                              add_numbers = TRUE, numbers_gp = gpar(fontsize = 12), numbers_rot=0,
                              annotation_name_gp=gpar(fontsize = 16)),
                          right_annotation = upset_right_annotation(M, 
                              add_numbers = TRUE, numbers_gp = gpar(fontsize = 10),
                              gp = gpar(fill = "blue"),
                              annotation_name_gp=gpar(fontsize = 16),
                              annotation_name_side = "top",
                              axis_param = list(side = "top"))
           ))
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("UpSetPlot:\n", e ); })
    })

    #----------------------------------------------------
    # renderPlot - Venn Diagram
    #----------------------------------------------------
    # https://statisticsglobe.com/ggvenn-r-package
    #
    output$VennPlot <- renderPlot({
       values$initdss
       tryCatch({
           if (length(input$inDSselect)<2 || length(input$inDSselect)>4) return(NULL)
           refID <- g$samples
           L <- list()
           for (setName in input$inDSselect) {
               data <- getData(ws, paste('(',setName,')',sep=''))
               L[[setName]] <- unique(sort(data[, refID]))
           }
           gg <- ggvenn(L, show_percentage = FALSE, set_name_size = 5,
                        fill_color = c("#F4FAFE", "#4981BF", "blue", "darkblue"))
           gg
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("VennPlot:\n", e ); })
    })

    output$infoVenn <- renderUI ({
       values$initdss
       input$inDSselect
       tryCatch({
           HTML(paste("<center><h4>Total number of variables =",nrow(g$varnames),"</h4></center>"))
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("VennPlot:\n", e ); })
    })

    #----------------------------------------------------
    # renderUI - Subsets Graph with d3js
    #----------------------------------------------------
    netReactive <- eventReactive(list(values$initdss,input$IdMenu), {
       if (nchar(input$ipclient)==0) return(NULL)
       if (! input$IdMenu %in% c('information','intersection')) return(NULL)
       tryCatch({ 
           if (length(g$subsetNames)>0) {
                diagonalNetwork(List = g$dn, fontSize = g$fs, fontFamily = "serif", 
                    linkColour = '#B2B3D0', nodeColour = "#fff", nodeStroke = "red", textColour = "darkblue",
                    opacity = 0.99)
           }
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderNetwork:\n", e ); })
    })

    output$Net <-  renderDiagonalNetwork({  netReactive()  })


    #----------------------------------------------------
    # renderUI - Wait for initialisation evenement 
    #----------------------------------------------------
    output$wait <- renderUI({
        values$init
        if (values$init==1) { "" }
    })



    #----------------------------------------------------
    # Rodam session within the About tab
    #----------------------------------------------------
    # TODO : add a copy-to-clipboard functionality. See https://cran.r-project.org/web/packages/rclipboard/readme/README.html
    output$sessioninfo <- renderPrint({
       values$init
       tryCatch({
          if (nchar(ws$dsname)>0) {
            authstr <- ''
            if (nchar(ws$auth)>0)
                authstr <- ifelse( ws$keymode==2, ", 'YOUR_SECRET_API_KEY'", paste0(", '", ws$auth,"'") );
            odamws_params <- paste0("'",ws$apiurl,"', '",ws$dsname,"'", authstr);
            cat("\noptions(width=256)\n", "options(warn=-1)\n","options(stringsAsFactors=FALSE)\n","\n",
                "library(Rodam)\n","\n",
                "# Initialize the 'ODAM' object \n", "dh <- new('odamws',",odamws_params,", maxtime=30)\n","\n",
                "# Get the Data Tree\n","show(dh)\n","\n",
                "# Get the data subsets list\n","dh$subsetNames\n","\n",
                sep=""
            )
            if ( values$launch>0) {
               if (length(input$inDSselect)>1) {
                  setName <- paste0("c(",paste(simplify2array(lapply(input$inDSselect, function(x) { paste0("'",x,"'") })),collapse=','),")")
               } else {
                  setName <- paste0("'",g$inDSselect,"'")
               }
               cat(
                  "# Get '",g$inDSselect,"' data subset\n", "ds <- dh$getSubsetByName(",setName,")\n", "\n",
                  "# Show the first 50 variable descriptions\n", "head(ds$LABELS, n=50)\n","\n",
                  "# Show all factors defined in the data subset\n","ds$facnames\n","\n",
                  "# Show the first 150 quantitative variables defined in the data subset\n","head(ds$varnames, 150)\n","\n",
                  "# Show all qualitative variables defined in the data subset\n","ds$qualnames\n","\n",
                  sep=""
               )
               if (length(input$inDSselect)>1)
                  cat("# Show qualitative variables by data subset\n","ds$varsBySubset\n","\n", sep="")
               cat(
                  "# Boxplot of all variables defined in ds$varnames\n",
                  "Rank <- simplify2array(lapply(ds$varnames, function(x) { round(mean(log10(ds$data[ , x]), na.rm=T)) }))\n",
                  "Rank[!is.finite(Rank)] <- 0\n",
                  "colRank <- Rank - min(Rank) + 1\n",
                  "cols <- c('red', 'orange', 'darkgreen', 'blue', 'purple', 'brown', 'darkblue', 'darkred', 'darkorange')\n",
                  "boxplot(log10(ds$data[, ds$varnames]), outline=F, horizontal=T, border=cols[colRank], las=2, cex.axis=0.5)\n",
                  "ColVars <- NULL\n",
                  "for(x in 1:length(ds$varsBySubset)) ColVars <- c(ColVars, rep(cols[x], length(ds$varsBySubset[[x]])))\n",
                  "for(x in 1:length(ColVars)) axis(side=2, at=x, col.axis=ColVars[x], labels= ds$varnames[x] , las=2, cex.axis=0.5)\n",
                  "\n", sep=""
               )
            }
          }
       }, error=function(e) { ERROR$MsgErrorAbout <- paste("RenderPrint:\n", e ); })
    })

   observe ({
       input$cp2clb
       if (input$cp2clb==0) return(NULL)
       runjs("copy2clipboard('sessioninfo');")
   })
