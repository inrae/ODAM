
    #----------------------------------------------------
    # Observer - Multivariate
    #----------------------------------------------------
    observe({
       if ( ! is.null(input$inDSelect) && input$inDSelect>0) {
          if ((as.numeric(input$nbComp)==2 && input$multiType=='ICA') || input$outType != 'IDS') {
              updateCheckboxInput(session, "f3D", label = '3D', value = FALSE)
              shinyjs::disable("f3D")
          } else {
              shinyjs::enable("f3D")
          }
       }
    })
    observe({
       if ( ! is.null(input$inDSelect) && input$inDSelect>0) {
            if (input$f3D==TRUE) {
                  shinyjs::disable("multiLabels")
                  shinyjs::disable("GBG")
            } else {
                  shinyjs::enable("multiLabels")
                  shinyjs::enable("GBG")
            }
       }
    })
    observe({
       if ( ! is.null(input$inDSelect) && input$inDSelect>0) {
          # Annotation
          fa_options <- c("None", .C(features[,2]))
          names(fa_options) <- c('---', .C(features$Description))
          updateSelectInput(session, "multiAnnot", choices = fa_options)
          updateSelectInput(session, "outType", selected="None")
          updateSelectInput(session, "multiType", selected="None")
       }
    })
    observe({
       if ( ! is.null(input$inDSelect) && input$inDSelect>0) {
          if (inDSelect != input$inDSelect) getVars(.N(input$inDSelect))
          # First Factor
          f1_options <- .C(facnames[,2])
          names(f1_options) <- .C(facnames$Description)
          if (dim(varnames)[1]>2) {
              updateSelectInput(session, "multiFacX", choices = f1_options)
          }
          # Select the variables to be included in the analysis
          v_options <- c( 1:dim(varnames)[1] )
          names(v_options) <- c(.C(varnames$Description))
          updateSelectInput(session, "listVars", choices = v_options, selected=v_options)
       }
    })
    observe({
       if (! is.null(input$inDSelect) && input$inDSelect>0 && ! is.null(input$multiFacX) && nchar(input$multiFacX)>0) {
          facvals <- data[ , input$multiFacX]
          if (is.numeric(facvals)) {
              fmt <- paste('%0',round(log10(max(abs(facvals))))+3,'.2f',sep='')
              facvals <- as.character(sprintf(fmt, facvals))
          }
          levelFac <- .C( levels(as.factor(facvals)) )
          l_options <- c( 1:length(levelFac) )
          names(l_options) <- c(as.character(c(levelFac)))
          updateSelectInput(session, "listLevels", choices = l_options, selected=l_options)
       }
    })
    observe({
       if (! is.null(input$inDSelect) && input$inDSelect>0 && ! is.null(input$multiAnnot) && nchar(input$multiAnnot)>0) {
          f_options <- c(.C(input$multiAnnot))
          names(f_options) <- c('---')
          if (.C(input$multiAnnot) != "None") {
              fvals <- data[ , input$multiAnnot]
              if (is.numeric(fvals)) {
                 fmt <- paste('%0',round(log10(max(abs(fvals))))+3,'.2f',sep='')
                 fvals <- as.character(sprintf(fmt, fvals))
              }
              flevels <- .C( levels(as.factor(fvals)) )
              if (length(flevels)<nbopt_multiselect) {
                  f_options <- c( 1:length(flevels) )
                  names(f_options) <- c(as.character(c(flevels)))
              }
          }
          updateSelectInput(session, "listFeatures", choices = f_options, selected=f_options)
       }
    })

    #----------------------------------------------------
    # renderUI - Multivariate : HCA / MDS
    #----------------------------------------------------

    output$Msg <- renderUI({
        if (input$inDSelect==0) return(NULL)
        if (input$inDSelect>0 && dim(varnames)[1]<3) {
            tags$p(class="shiny-output-error","Not enough variables")
        }
    })

    output$MultiPlot <- renderPlotly ({
        if (input$inDSelect==0) return(NULL)
        input$listVars
        input$listLevels
        input$listFeatures
        input$viewComp
        input$nbComp
        FA <- isolate(input$multiAnnot)
        F1 <- .C(isolate(input$multiFacX))
        outputVariables <- ifelse( input$outType == 'IDS', FALSE, TRUE )
        FCOL <- ifelse( FA=="None", '', FA )
        selectFCOL <- input$listFeatures
        if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
            selectFCOL <- c()
        }
        withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
            tryCatch({
                getMultiPLot(input$multiType, F1, selectLevels=input$listLevels, FCOL=FCOL, selectFCOL=selectFCOL, selectVars=input$listVars,
                             outputVariables, ellipse=input$ellipse, scale=input$scale, blabels=input$multiLabels, f3D=input$f3D, GBG=input$GBG)
            }, error=function(e) {})
        })
    })

    #----------------------------------------------------
    # Multivariate : PCA
    #----------------------------------------------------
    PCA_fun <- function(x) {
    
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
    ICA_fun <- function(x) {
    
        Kurtosis_order <- F
        nbc <- as.numeric(input$nbComp)
        out.ica <- JADE(x, nbc, maxiter = 200)
        Score <- out.ica$S
        M <- t(x)%*%Score
        colnames(Score)<-colnames(M)<-paste("IC", 1:nbc,sep="")
        
        KS <- kurtosis(Score)
        KSord <- order(KS,decreasing = Kurtosis_order)
        
        pc1 <- KSord[1]
        pc2 <- KSord[2]
        pc3 <- ifelse(input$nbComp>2, KSord[3], pc1 )
        evnorm <- NULL
        for (i in 1:nbc) {
            evnorm <- c( evnorm, 100*sum(cor(Score[,i],x)^2)/dim(x)[2] )
        }
        if (evnorm[pc1]<evnorm[pc2]) { pc1 <- KSord[2]; pc2 <- KSord[1]; }
        list(Score=Score, Loadings=M, pc1=pc1, pc2=pc2, pc3=pc3, evnorm=evnorm, prefix='IC' )
    }
    
    #----------------------------------------------------
    # Multivariate Plot
    #----------------------------------------------------
    getMultiPLot <- function(Analysis, F1, selectLevels, FCOL, selectFCOL, selectVars, 
                              outputVariables=FALSE, ellipse=TRUE, scale=FALSE, blabels=TRUE, f3D=FALSE, GBG=FALSE) {
        FUN <- ''
        if (nchar(Analysis)>0) FUN <- paste0(Analysis,'_fun')
        if ( ! exists(FUN) ) return(NULL)
    
        # Metadata preparation
        F1name <- as.character(LABELS[LABELS[,1]==F1,2])
        variables <-.C(varnames[.N(selectVars),]$Attribute)
    
        facvals <- data[ , F1]
        if (is.numeric(facvals)) {
            fmt <- paste('%0',round(log10(max(abs(facvals))))+3,'.2f',sep='')
            facvals <- as.character(sprintf(fmt, facvals))
        }
        levelFac <- .C( levels(as.factor(facvals)) )
    
        fannot=TRUE
        if (is.null(FCOL) || nchar(FCOL)==0) { FCOL <- F1; fannot=FALSE; }
        FCOL <- tryCatch( { if(length(data[, FCOL ])) FCOL  }, error=function(e) { F1 })
        cfacvals <- as.vector(data[ , FCOL])
        cfacvals[is.na(cfacvals)] <- "NA"
        if (is.numeric(cfacvals)) {
            fmt <- paste('%0',round(log10(max(abs(cfacvals))))+3,'.2f',sep='')
            cfacvals <- as.character(sprintf(fmt, cfacvals))
        }
        levelcFac <- .C( levels(as.factor(cfacvals)) )
    
        # Data extraction
        subdata <- na.omit(cbind( data[ , c(samples,variables)] , facvals, cfacvals ))
        colnames(subdata) <- c ( samples, variables, F1, FCOL)
        subdata <- subdata[subdata[ , F1 ] %in% levelFac[.N(selectLevels)], ]
        if (fannot && length(selectFCOL)>0) subdata <- subdata[subdata[ , FCOL ] %in% levelcFac[.N(selectFCOL)], ]
        facvals <- subdata[, F1 ]
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
           
           if (! f3D) {
              centroids <- aggregate(cbind(C1,C2) ~ fac , MA, mean)
              conf.rgn  <- do.call(rbind,lapply(unique(MA$fac),function(t)
                data.frame(fac=as.character(t),
                           ellipse(cov(MA[MA$fac==t,1:2]),
                                   centre=as.matrix(centroids[centroids$fac==t,2:3]),
                                   level=0.95, npoints=50),
                           stringsAsFactors=FALSE)))
              for( i in 1:length(levels(facvals)) ) conf.rgn$fac[ conf.rgn$fac==i ] <- levels(facvals)[i]
           }
           if (fannot) {
               MA$IDS <- sapply( subdata[ , samples], function (x) { paste(unique(as.vector(subdata[subdata[ , samples]==x, FCOL])),sep="", collapse=',')} )
           } else {
               MA$IDS <- subdata[, samples ]
           }
           names(MA) <- c( 'C1','C2', 'C3', 'fac', 'IDS')
           for( i in 1:length(levels(facvals)) ) MA$fac[ MA$fac==i ] <- levels(facvals)[i]
           MA$fac <- as.factor(MA$fac)

           if (f3D) {
              symbolset = c('dot', 'cross', 'diamond', 'square', 'triangle-down', 'triangle-left', 'triangle-right', 'triangle-up')
              gg <- plot_ly(MA, x = ~C1, y = ~C2, z = ~C3, color = ~fac, 
                 type="scatter3d", marker=list(size = 4), text= ~IDS ) %>%
                 layout(scene = list(xaxis = list(title = sprintf("%s%d = %6.2f%%",prefix, pc1, evnorm[pc1])),
                       yaxis = list(title = sprintf("%s%d = %6.2f%%",prefix, pc2, evnorm[pc2])),
                       zaxis = list(title = sprintf("%s%d = %6.2f%%",prefix, pc3, evnorm[pc3]))))    
           } else {
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
              if (ellipse) G1 <- G1 + geom_path(data=conf.rgn)
              G1 <- G1 + guides(colour = guide_legend(override.aes = list(size=3)))
              if (!GBG) G1 <- G1 + theme_bw()
              gg <- ggplotly(G1)
           }

        } else {

        # Loadings plot
           
           MA <- as.data.frame(M[, c(pc1,pc2)])
           names(MA) <- c( 'C1','C2' )
           MA$VARS <- rownames(M)
           MA$LABELS <- as.character(LABELS[LABELS[,1] %in% MA$VARS,2])
           strmode <- ifelse( blabels==TRUE, "text", "markers" )
           names(MA) <- c( 'C1','C2', 'VARS', 'LABELS' )
           gg <- plot_ly(MA, x = ~C1, y = ~C2, color = "blue", type="scatter", mode=strmode, text= ~LABELS ) %>%
                 layout(scene = list(xaxis = list(title = sprintf("%s%d",prefix, pc1)),yaxis = list(title = sprintf("%s%d",prefix, pc2))))
        }
        #htmlwidgets::saveWidget(as.widget(gg), file = "getMultiPLot.html")
        gg
    }

