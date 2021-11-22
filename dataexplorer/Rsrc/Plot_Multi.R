    #----------------------------------------------------
    # Observer - ERROR
    #----------------------------------------------------
    observe ({
       ERROR$MsgErrorMulti
       if (nchar(ERROR$MsgErrorMulti)>0) {
          createAlert(session, "ErrAlertMulti", "ErrAlertMultiId", title = "", content = ERROR$MsgErrorMulti, append = FALSE, style='danger')
       }
    })


    #----------------------------------------------------
    # Observer - Multivariate
    #----------------------------------------------------
    observeEvent ( values$launch, { tryCatch({
       if ( values$launch>0 && ui$type %in% c('PCA','ICA','COR','GGM') ) {
           v_options <- c('PCA','ICA','COR','GGM')
           names(v_options) <- c("Principal Component Analysis (PCA)", "Independent Component Analysis (ICA)",
                                 "Heatmap of correlation matrix (COR)", "Gaussian graphical model (GGM)")
           updateSelectInput(session, "multiType", choices = v_options,  selected=ui$type)
           values$multitype <- ui$type
           ui$type <<- ''
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 0:\n", e ); }) })


    # multiType event
    observe({ tryCatch({
       input$inDselect
       input$multiType
       if ( values$launch>0 ) {
          values$outtype <- 'None'
          values$multitype <- .C(input$multiType)
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 1a:\n", e ); }) })

    # On multiType event, change outType
    observe({ tryCatch({
       input$inDselect
       values$multitype
       if ( values$launch>0 && values$multitype %in% c('PCA','ICA','COR','GGM') ) {
          if ( values$multitype %in% c('COR','GGM') ) {
             v_options <- c('VARS')
             names(v_options) <- c('Variables')
             updateSelectInput(session, "outType", choices = v_options,  selected="VARS")
             values$outtype <- 'VARS'
          } else {
             v_options <- c('IDS','VARS')
             names(v_options) <- c('Identifiers', 'Variables')
             updateSelectInput(session, "outType", choices = v_options,  selected="IDS")
             values$outtype <- 'IDS'
          }
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 1b:\n", e ); }) })

    # outType event
    observe({ tryCatch({
       input$inDselect
       input$outType
       if ( values$launch>0 )
          values$outtype <- .C(input$outType)
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 1c:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 ) {
          if ((as.numeric(input$nbComp)==2 && values$multitype=='ICA') || values$multitype %in% c('COR','GGM') || values$outtype != 'IDS' ) {
              updateCheckboxInput(session, "f3D", label = '3D', value = FALSE)
              shinyjs::disable("f3D")
          } else {
              shinyjs::enable("f3D")
          }
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 2:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 ) {
            if (input$f3D==TRUE) {
                shinyjs::disable("multiLabels")
                shinyjs::disable("GBG")
            } else {
                shinyjs::enable("multiLabels")
                shinyjs::enable("GBG")
            }
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 3:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0) {
          if (nrow(g$varnames)>maxVariables) return(NULL)
          # First Factor
          f1_options <- .C(g$facnames[,2])
          names(f1_options) <- .C(g$facnames$Description)
          selFacX <- f1_options[1]
          if (! is.null(ui$fac1) && ! is.na(ui$fac1) && nchar(ui$fac1)>0 && ui$fac1 %in% .C(g$facnames[,3]))
              selFacX <- g$facnames[g$facnames[,3]==ui$fac1,2]
          if (nrow(g$varnames)>2)
              updateSelectInput(session, "multiFacX", choices = f1_options, selected=selFacX)
          # Select the variables to be included in the analysis
          v_options <- c( 1:nrow(g$varnames) )
          names(v_options) <- c(.C(gsub(" \\(.+\\)","",.C(g$varnames$Description))))
          updateSelectInput(session, "listVars", choices = v_options, selected=v_options)
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 4:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if (values$launch>0 && ! is.null(input$multiFacX) && nchar(.C(input$multiFacX))>0) {
          facvals <- g$data[ , .C(input$multiFacX)]
          if (is.numeric(facvals) && sum(facvals-floor(facvals))>0) {
              fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
              facvals <- as.character(sprintf(fmt, facvals))
          }
          levelFac <- .C( levels(as.factor(facvals)) )
          l_options <- c('')
          names(l_options) <- c('---')
          if (length(levelFac)<nbopt_multiselect) {
              l_options <- c( 1:length(levelFac) )
              names(l_options) <- c(as.character(c(levelFac)))
          }
          updateSelectInput(session, "listLevels", choices = l_options, selected=l_options)
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 5:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0) {
          # Annotation
          fa_options <- c("None", .C(g$features[,2]))
          names(fa_options) <- c('---', .C(g$features$Description))
          updateSelectInput(session, "multiAnnot", choices = fa_options)
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 6:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if (values$launch>0 && ! is.null(input$multiAnnot) && nchar(input$multiAnnot)>0) {
          f_options <- c('')
          names(f_options) <- c('---')
          if (input$multiAnnot != "None") {
              fvals <- g$data[ , input$multiAnnot]
              fident <- ifelse( input$multiAnnot %in% g$identifiers$Attribute, TRUE, FALSE)
              if (!fident && is.numeric(fvals) && sum(is.na(fvals))==0 && sum(fvals-floor(fvals))>0) {
                 fmt <- paste('%0',round(log10(max(abs(fvals)))+0.5)+3,'.2f',sep='')
                 fvals <- as.character(sprintf(fmt, fvals))
              }
              flevels <- levels(as.factor(fvals))
              if (length(flevels)<nbopt_multiselect) {
                  f_options <- c( 1:length(flevels) )
                  names(f_options) <- c(as.character(c(flevels)))
              }
          }
          updateSelectInput(session, "listFeatures", choices = f_options, selected=f_options)
       }
    }, error=function(e) { ERROR$MsgErrorMulti <- paste("Observer 7:\n", e ); }) })


#===================================================================================


    #----------------------------------------------------
    # GGM
    #----------------------------------------------------
    observeEvent(c(
        values$launch,
        values$multitype,
        values$outtype,
        input$shrinkauto,
        input$lambda,
        input$multiLog,
        input$qval,
        input$multiAnnot,
        input$multiFacX,
        input$listFeatures,
        input$listLevels,
        input$listVars
    ),{ tryCatch({ ERROR$MsgErrorMulti <- ''; closeAlert(session, "ErrAlertMultiId")
           if (values$launch==0) return(NULL)
           if (values$multitype != 'GGM') return(NULL)
           if (values$outtype != 'VARS') return(NULL)
           FA <- .C(isolate(input$multiAnnot))
           F1 <- .C(isolate(input$multiFacX))
           FCOL <- ifelse( FA=="None", '', FA )
           selectFCOL <- .C(input$listFeatures)
           if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
               selectFCOL <- c()
           }
           withProgress(message = 'GGM Calculation in progress', detail = '... ', value = 0, {
              tryCatch({
                 ## Metadata preparation / Data extraction
                 o <- getDataMulti(F1, .C(input$listLevels), FCOL, selectFCOL, .C(input$listVars), scale=TRUE)
                 X <- unique( as.matrix(o$subdata[, o$variables]) )
                 if (input$multiLog) X <- log10(abs(X)+pseudo_zero)*sign(X)
                 n <- nrow(X)
                 p <- ncol(X)

                 # Depending on shrink mode, check/estimate  shrinkage intensity lambda
                 lambda <- input$lambda
                 if (input$shrinkauto>0 || lambda<0) {
                     lambda<-sqrt(2*log(p/sqrt(n))/n)
                 }

                 library(RcppParallel)
                 setThreadOptions(numThreads = 4)
                 out <- FastGGM_Parallel(X, lambda)

                 R <-  out$partialCor
                 colnames(R) <- colnames(X)
                 rownames(R) <- colnames(X)

                 # Adjust pvalues
                 P <- p.adjust(out$p_partialCor,method = 'fdr')
                 Pm <- matrix(data=P,nrow=nrow(R),ncol=ncol(R),byrow=TRUE)
                 colnames(Pm) <- colnames(X)
                 rownames(Pm) <- colnames(X)
                 P <- Pm

                 # Upper correlation matrix
                 cor_mat <- R
                 cor_mat[ lower.tri(cor_mat, diag=TRUE) ]<- 0

                 # Threshold applied on p-values
                 qval <- input$qval
                 P[ P > qval ] <- 0
                 P[ is.na(P) ] <- 0
                 cor_mat[ P==0 ] <- 0

                 # Generate the full correlation graph
                 graph <- graph.adjacency(cor_mat!=0, weighted=TRUE, mode="upper")

                 # Init. features of edges
                 E(graph)$weight<-t(cor_mat)[t(cor_mat)!=0]
                 E(graph)[ weight<0 ]$color <- "green"
                 E(graph)[ weight>0 ]$color <- "red"

                 # Init. features of vertices
                 VS <- 1
                 V(graph)$size<-rep(VS,length(V(graph)))
                 V(graph)$label<- V(graph)$name

                 values$netData <<- data.frame(source=as_edgelist(graph)[,1], target=as_edgelist(graph)[,2], Corr=t(cor_mat)[t(cor_mat)!=0])
             }, error=function(e) { ERROR$MsgErrorMulti <- paste("FastGGM :\n", e ); })
           })
        }, error=function(e) { ERROR$MsgErrorMulti <- paste("observeEvent:\n", e ); })
    })

    output$ggmnet <- renderForceNetwork({
       tryCatch({ ERROR$MsgErrorMulti <- ''; closeAlert(session, "ErrAlertMultiId")
           if (values$launch==0) return(NULL)
           if (values$multitype != 'GGM') return(NULL)
           if (values$outtype != 'VARS') return(NULL)
           values$netData
           input$gravite
           netData <- values$netData
           Vertices <- unique(sort(c( unique(sort(as.vector(netData[,1]))),  unique(sort(as.vector(netData[,2]))) )))
           V_size <- length(Vertices)
           E_size <- nrow(netData)
           Nodesize <- simplify2array(lapply( 1:V_size, function(x) { sum(Vertices[x] == c(as.vector(netData[,1]), as.vector(netData[,2])))^2 }))

           # Groups for Nodes based on data subsets
           dsSel <- names(g$varsBySubset)
           dsnames <- rep(dsSel[1], length(Vertices))
           if (length(dsSel)>1) {
                for(i in 2:length(dsSel))
                    dsnames[ Vertices %in% g$varsBySubset[[i]] ] <- dsSel[i]
           }

           # Change default colors
           jsCols <- paste(sapply(c('#1f77b4','#ff7f0e','#2ca02c','#d62728','#9467bd','#8c564b','#e377c2','#7f7f7f','#bcbd22','#17becf'),
                           function(x) { paste0("d3.rgb(", paste(c(col2rgb(x), 0.5), collapse = "," ), ")") }), collapse = ", ")
           colorJS <- paste0('d3.scale.ordinal().range([', jsCols, '])')

           L1<-simplify2array(lapply( 1:E_size, function(x) { which( as.vector(netData[,1])[x]==Vertices) }))
           L2<-simplify2array(lapply( 1:E_size, function(x) { which( as.vector(netData[,2])[x]==Vertices) }))
           Links <- data.frame( source=(L1-1), target=(L2-1), value=10*( abs(netData$Corr)-0.3 ) )
           Nodes <- data.frame( name=Vertices, group=dsnames, size=Nodesize )
           LS <- 1
           link_colors <- rep('Red', E_size); link_colors[which(netData$Corr<0)] <- 'Green'
           fontSize <- 10+5*(LS-1)
           logcharge <- 10*input$gravite-7
           charge <- sign(logcharge)*10^(abs(logcharge))

           outfile <- file.path(SESSTMPDIR,outfiles[[values$multitype]])
           fn <- forceNetwork(Links = Links, Nodes = Nodes, Source = "source", Target = "target", Value = "value", NodeID = "name", Group = "group",
                       Nodesize="size", fontSize=fontSize, linkColour=link_colors, charge=charge, colourScale=JS(colorJS),
                       opacity = 0.99, opacityNoHover = 0.8, zoom = TRUE, bounded=TRUE, legend=TRUE)
            if (saveplots) saveNetwork(fn, file = outfile)
           fn
       }, error=function(e) { ERROR$MsgErrorMulti <- paste("RenderForceNetwork:\n", e ); })
    })


#===================================================================================


    #----------------------------------------------------
    # renderUI - Multivariate : PCA / ICA
    #----------------------------------------------------

    output$Msg <- renderUI({
        if (values$launch==0) return(NULL)
        if (values$launch>0 && nrow(g$varnames)<3) {
            tags$p(class="shiny-output-error","Not enough variables")
        }
    })

    # Render PCA & ICA Plotly
    output$MultiPlot <- renderPlotly ({
       tryCatch({ ERROR$MsgErrorMulti <- ''; closeAlert(session, "ErrAlertMultiId")
           if (values$launch==0) return(NULL)
           if (! values$multitype %in% c('PCA','ICA')) return(NULL)
           if (! values$outtype %in% c('IDS','VARS')) return(NULL)
           input$listVars
           input$listLevels
           input$listFeatures
           input$viewComp
           input$nbComp
           FA <- .C(isolate(input$multiAnnot))
           F1 <- .C(isolate(input$multiFacX))
           outputVariables <- ifelse( values$outtype == 'IDS', FALSE, TRUE )
           FCOL <- ifelse( FA=="None", '', FA )
           selectFCOL <- .C(input$listFeatures)
           if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
               selectFCOL <- c()
           }
           multiType <- values$multitype
           withProgress(message = paste0(values$multitype,' Calculation in progress'), detail = '... ', value = 0, {
               tryCatch({
                  getMultiPlot(multiType, F1, .C(input$listLevels), FCOL, selectFCOL, .C(input$listVars), outputVariables=outputVariables,
                               fellipse=input$ellipse, scale=input$scale, blabels=input$multiLabels, slabels=input$shortLabels,
                               f3D=input$f3D, GBG=input$GBG,conflevel=as.numeric(input$conflevel))
               }, error=function(e) { ERROR$MsgErrorMulti <- paste("getMultiPlot :\n", e ); })
           })
       }, error=function(e) { ERROR$MsgErrorMulti <- paste("RenderPlotly:\n", e ); })
    })

    #----------------------------------------------------
    # Multivariate : PCA
    #----------------------------------------------------
    PCA_fun <- function(x)
    {
        pc<-prcomp(x,retx=TRUE,scale=F)
        sd <- pc$sdev
        eigenvalues <- sd^2
        evnorm <- (100*eigenvalues/sum(eigenvalues))[1:10]
        Score <- pc$x
        M <- pc$rotation
        if (input$viewComp=="1") { pc1 <- 1;  pc2 <- 2; pc3 <- 3; }
        if (input$viewComp=="2") { pc1 <- 1;  pc2 <- 3; pc3 <- 2; }
        if (input$viewComp=="3") { pc1 <- 2;  pc2 <- 3; pc3 <- 1; }

        list(Score=Score, Loadings=M, pc1=pc1, pc2=pc2, pc3=pc3, evnorm=evnorm, prefix='PC' )
    }

    #----------------------------------------------------
    # Multivariate : ICA
    #----------------------------------------------------
    ICA_fun <- function(x)
    {
        nbc <- as.numeric(input$nbComp)
        out.ica <- JADE(x, nbc, maxiter = 200)
        Score <- out.ica$S
        M <- t(x)%*%Score
        colnames(Score)<-colnames(M)<-paste("IC", 1:nbc,sep="")

        # Kurtosis_order <- F
        # KS <- kurtosis(Score)
        # KSord <- order(KS,decreasing = Kurtosis_order)
        # pc1 <- KSord[1]
        # pc2 <- KSord[2]
        # pc3 <- ifelse(input$nbComp>2, KSord[3], pc1 )

        evnorm <- NULL
        for (i in 1:nbc) {
            evnorm <- c( evnorm, 100*sum(cor(Score[,i],x)^2)/ncol(x) )
        }
        evord <- order(evnorm, decreasing=T)
        pc1 <- evord[1]
        pc2 <- evord[2]
        pc3 <- ifelse(input$nbComp>2, evord[3], pc1 )

        list(Score=Score, Loadings=M, pc1=pc1, pc2=pc2, pc3=pc3, evnorm=evnorm, prefix='IC' )
    }


    #----------------------------------------------------
    # Multivariate Plot
    #----------------------------------------------------
    getMultiPlot <- function(Analysis, F1, selectLevels, FCOL, selectFCOL, selectVars,
                             outputVariables=FALSE, fellipse=TRUE, scale=FALSE, blabels=TRUE, slabels=FALSE, f3D=FALSE, GBG=FALSE, conflevel=0.95)
    {
        FUN <- ''
        if (nchar(Analysis)>0) FUN <- paste0(Analysis,'_fun')
        if ( ! exists(FUN) ) return(NULL)

        ## Metadata preparation / Data extraction
        o <- getDataMulti(F1, selectLevels, FCOL, selectFCOL, selectVars, scale=F)
        subdata<-o$subdata; facvals<-o$facvals; variables<-o$variables; F1name<-o$F1name; fannot<-o$fannot;

        x <- subdata[, variables ]
        if (scale) x <- scale(x)

        out <- do.call(FUN, list(x))
        Score <- out$Score
        M <- out$Loadings
        pc1 <- out$pc1
        pc2 <- out$pc2
        pc3 <- out$pc3
        evnorm <- out$evnorm
        prefix <- out$prefix

        if (outputVariables==FALSE) {

        # Scores plot
           MA <- as.data.frame(cbind( Score[, c(pc1,pc2,pc3)], facvals))
           names(MA) <- c( 'C1','C2', 'C3', 'fac' )
           if (fannot) {
               MA$IDS <- sapply( subdata[ , g$samples], function (x) { paste(unique(as.vector(subdata[subdata[ , g$samples]==x, FCOL])),sep="", collapse=',')} )
           } else {
               MA$IDS <- subdata[, g$samples ]
           }
           names(MA) <- c( 'C1','C2', 'C3', 'fac', 'IDS')
           for( i in 1:length(levels(as.factor(facvals))) ) MA$fac[ MA$fac==i ] <- levels(as.factor(facvals))[i]
           MA$fac <- as.factor(MA$fac)
           MA <- unique(MA)

#write.table(MA, file = file.path(tempdir(),'MA.txt'), append = FALSE, quote = TRUE, sep = "\t", na = "NA", dec = ".", row.names = FALSE)

           if (f3D) { # Use 3D plotly
              symbolset = c('dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up')
              gg <- plot_ly(MA, x = ~C1, y = ~C2, z = ~C3, color = ~fac,
                 type="scatter3d", marker=list(size = 4), text= ~IDS ) %>%
                 layout(scene = list(xaxis = list(title = sprintf("%s%d = %6.2f%%",prefix, pc1, evnorm[pc1])),
                       yaxis = list(title = sprintf("%s%d = %6.2f%%",prefix, pc2, evnorm[pc2])),
                       zaxis = list(title = sprintf("%s%d = %6.2f%%",prefix, pc3, evnorm[pc3]))))
           } else { # Use 2D ggplot / plotly
              sizeP <- ifelse( blabels, 0, 0 )
              G1 <- ggplot(data=MA,(aes(x=C1,y=C2,colour = fac)))
              if (!blabels) G1 <- G1 + geom_point(size=sizeP)
              if (blabels) G1 <- G1 + geom_text(aes(label=IDS),hjust=0.5,vjust=0.5)
              if (! is.null(evnorm) ) {
                  G1 <- G1 + xlab(sprintf("%s%d = %6.2f%%",prefix, pc1, evnorm[pc1]))
                  G1 <- G1 + ylab(sprintf("%s%d = %6.2f%%",prefix, pc2, evnorm[pc2]))
              } else {
                  G1 <- G1 + xlab(sprintf("%s%d",prefix, pc1))
                  G1 <- G1 + ylab(sprintf("%s%d",prefix, pc2))
              }
              G1 <- G1 + labs(colour=F1name)
              if (fellipse) G1 <- G1 + stat_ellipse(type='norm', level=conflevel, na.rm=TRUE)
              G1 <- G1 + guides(colour = guide_legend(override.aes = list(size=3)))
              if (!GBG) G1 <- G1 + theme_bw()
              gg <- G1 #ggplotly(G1)
           }

        } else {

        # Loadings plot - 2D plotly

           ## Label colors
           dsSel <- names(g$varsBySubset)
           dsnames <- rep(dsSel[1], length(rownames(M)))
           if (length(dsSel)>1) {
               for(i in 1:length(dsSel))
                  dsnames[ rownames(M) %in% g$varsBySubset[[i]] ] <- dsSel[i]
           }

           idlabels <- ifelse (slabels, 2, 4)
           MA <- as.data.frame(M[, c(pc1,pc2)])
           MA <- cbind( MA, rownames(M) )
           names(MA) <- c( 'C1','C2', 'VARS' )
           MA <- cbind( MA, gsub(" \\(.+\\)","",as.character(g$LABELS[g$LABELS[,2] %in% MA$VARS,idlabels])) )
           names(MA) <- c( 'C1','C2', 'VARS', 'LABELS' )
           if (length(dsSel)>1) {
              MA <- cbind( MA, dsnames ); fshow <- TRUE
           } else {
              MA <- cbind( MA, MA$LABELS ); fshow <- FALSE
           }
           names(MA) <- c( 'C1','C2', 'VARS', 'LABELS', 'COLORS' )
           strmode <- ifelse( blabels==TRUE, "text", "markers" )
           gg <- plot_ly(MA, x = ~C1, y = ~C2, color = ~COLORS, type="scatter", mode=strmode, text= ~LABELS ) %>%
                 layout(showlegend = fshow, scene = list(xaxis = list(title = sprintf("%s%d",prefix, pc1)),yaxis = list(title = sprintf("%s%d",prefix, pc2))))
        }
        #htmlwidgets::saveWidget(as.widget(gg), file = file.path(SESSTMPDIR,outfiles[[Analysis]]))
        gg
    }


#===================================================================================


    #----------------------------------------------------
    # renderUI - Multivariate : COR
    #----------------------------------------------------

    # Render COR Plot
    output$CorrPlot <- renderImage ({
       tryCatch({ ERROR$MsgErrorMulti <- ''; closeAlert(session, "ErrAlertMultiId")
           if (values$launch==0) return(NULL)
           if (! values$multitype %in% c('COR')) return(NULL)
           if (! values$outtype %in% c('VARS')) return(NULL)
           input$listVars
           input$listLevels
           input$listFeatures
           FA <- .C(isolate(input$multiAnnot))
           F1 <- .C(isolate(input$multiFacX))
           FCOL <- ifelse( FA=="None", '', FA )
           selectFCOL <- .C(input$listFeatures)
           if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
               selectFCOL <- c()
           }
           imgObj <- NULL
           withProgress(message = 'CORR Calculation in progress', detail = '... ', value = 0, {
               tryCatch({
                  imgObj <- getCorrPlot(F1, selectLevels=.C(input$listLevels), FCOL=FCOL, selectFCOL=selectFCOL, selectVars=.C(input$listVars),
                             methcor=input$methcor, blog=input$multiLog, full=input$fullmatcor, reorder=input$reordermatcor)
               }, error=function(e) { ERROR$MsgErrorMulti <- paste("getCorrPlot :\n", e ); })
           })
           imgObj
       }, error=function(e) { ERROR$MsgErrorMulti <- paste("RenderImage:\n", e ); })
    })

    #----------------------------------------------------
    # Heatmap of correlation matrix
    #----------------------------------------------------
    getCorrPlot <- function(F1, selectLevels, FCOL, selectFCOL, selectVars, methcor='pearson', blog=FALSE, full=FALSE, reorder=TRUE)
    {
        ## Metadata preparation / Data extraction
        o <- getDataMulti(F1, selectLevels, FCOL, selectFCOL, selectVars, scale=FALSE)
        x <- unique( as.matrix(o$subdata[, o$variables]) )
        if (blog) x <- log10(abs(x)+pseudo_zero)*sign(x)

        ## Correlation
        methcor <- ifelse(!methcor %in% c("pearson","kendall","spearman"), "pearson", methcor)
        cormat <- round(cor(x, method=methcor),2)
        ## Reorder by clustering
        if (reorder) {
           dd <- as.dist((1-cormat)/2)
           hc <- hclust(dd)
           cormat <-cormat[hc$order, hc$order]
        }
        if (!full) cormat[lower.tri(cormat)]<- NA
        melted_cormat <- melt(cormat, na.rm = TRUE)

        ##  Coloring the axis tick text by multiple colors - Seems not working with the R plotly package
        ## https://stackoverflow.com/questions/57486547/coloring-the-axis-tick-text-by-multiple-colors/58643916#58643916
        labcolors <- rep('black', length(o$variables))
        dsSel <- names(g$varsBySubset)
        if (length(dsSel)>1) {
            cols <- c('red','blue','orange','magenta','brown')
            for(i in 1:length(dsSel))
               labcolors[ rownames(cormat) %in% g$varsBySubset[[i]] ] <- cols[i]
        }

        chars <- strsplit(methcor,'')[[1]]
        LegendLabel <- paste0(toupper(chars[1]),paste(chars[-1],collapse=''),"\nCorrelation")

        ## Graphic
        G1 <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))
        G1 <- G1 + geom_tile(color = "white") + xlab("") + ylab("")
        G1 <- G1 + scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                          midpoint = 0, limit = c(-1,1), space = "Lab", name=LegendLabel)
        G1 <- G1 + theme_minimal()
        G1 <- G1 + theme(aspect.ratio = 0.5, plot.margin = margin(0, 0, 0, 0, "cm"),
                         axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1, colour=labcolors),
                         axis.text.y = element_text(size = 6, colour=labcolors))
        G1 <- G1 + coord_fixed()

        # Save graphic as SVG
        width  <- session$clientData$output_CorrPlot_width
        height <- session$clientData$output_CorrPlot_height
        mysvgwidth <- width/90
        mysvgheight <- height/90

        outfile <- file.path(SESSTMPDIR,outfiles[['COR']])
        ggsave(file=outfile, plot=G1, width=mysvgwidth, height=mysvgheight)

        list(src = outfile,
           contentType = 'image/svg+xml',
           width = width,
           height = height,
           alt = ' SVG Image'
        )

    }

#===================================================================================


    #----------------------------------------------------
    # renderUI - URL link 
    #----------------------------------------------------

    output$urlimage <- renderUI({
       tryCatch({
          if ( values$launch==0 ) return(NULL)
          if ( ! values$multitype %in% c('COR','GGM') ) return(NULL)
          if ( ! saveplots) return(NULL)
          #if (! file.exists(file.path(SESSTMPDIR,outfiles[[values$multitype]])) ) return(NULL)
          myurl <- paste0(cdata$url_protocol,'//',cdata$url_hostname,cdata$url_pathname,'tmp/',SESSID,'/', outfiles[[values$multitype]])
          HTML( paste0('<center>',a(href=myurl,"Open in new Tab",target="_blank"),'</center>') )
       }, error=function(e) { ERROR$MsgErrorInfo <- paste("RenderText - urlimage \n", e ); })
    })


#===================================================================================


    ## Metadata preparation / Data extraction
    getDataMulti <- function(F1, selectLevels, FCOL, selectFCOL, selectVars, scale=FALSE ) {
        # Metadata preparation
        F1name <- as.character(g$LABELS[g$LABELS[,2]==F1,4])
        variables <-.C(g$varnames[.N(selectVars),]$Attribute)

        # Remove quantitative variables with all values at zero
        data <- g$data
        V <- simplify2array( lapply(variables, function(v) { sum( which(data[, v]!=0) ) }) )
        if (length(which(V==0))>0) {
           data <- data[, ! colnames(data) %in% variables[c(which(V==0))] ]
           variables <- variables[ -c(which(V==0)) ]
        }

        facvals <- data[ , F1]
        if (is.numeric(facvals) && sum(facvals-floor(facvals))>0) {
            fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
            facvals <- as.character(sprintf(fmt, facvals))
        }
        levelFac <- .C( levels(as.factor(facvals)) )

        fannot=TRUE
        if (is.null(FCOL) || nchar(FCOL)==0) { FCOL <- F1; fannot=FALSE; }
        FCOL <- tryCatch( { if(length(data[, FCOL ])) FCOL  }, error=function(e) { F1 })
        cfacvals <- as.vector(data[ , FCOL])
        ofacvals <- order(cfacvals)
        cfacvals[is.na(cfacvals)] <- "NA"
        fident <- ifelse( FCOL %in% g$identifiers$Attribute, TRUE, FALSE )
        if (! fident && is.numeric(cfacvals) && sum(cfacvals-floor(cfacvals))>0) {
            fmt <- paste('%0',round(log10(max(abs(cfacvals)))+0.5)+3,'.2f',sep='')
            cfacvals <- as.character(sprintf(fmt, cfacvals))
        }

        # Data extraction
        subdata <- cbind( data[ , c(g$samples,variables)] , facvals, cfacvals )
        if (fannot && length(selectFCOL)>0)
            subFCOL <- unique(subdata$cfacvals[ofacvals])[.N(selectFCOL)]
        #subdata <- na.omit(cbind( data[ , c(g$samples,variables)] , facvals, cfacvals ))

        # Data imputation
        dataIn <- subdata[, variables ]
        if (scale) dataIn <- scale( dataIn, center=TRUE, scale=TRUE )
        resNIPALS <- pca(as.matrix(dataIn), method = "nipals", center = FALSE)
        subdata[, variables ] <- resNIPALS@completeObs
        colnames(subdata) <- c ( g$samples, variables, F1, FCOL)

        # Data selection
        subdata <- subdata[subdata[ , F1 ] %in% levelFac[.N(selectLevels)], ]
        if (fannot && length(selectFCOL)>0) subdata <- subdata[subdata[ , FCOL ] %in% subFCOL, ]
        facvals <- subdata[, F1 ]
        list( subdata=subdata, facvals=facvals, variables=variables, F1name=F1name, fannot=fannot )
    }

