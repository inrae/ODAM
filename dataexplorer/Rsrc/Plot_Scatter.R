    #----------------------------------------------------
    # Observer - ERROR
    #----------------------------------------------------
    observe ({
       ERROR$MsgErrorBi
       if (nchar(ERROR$MsgErrorBi)>0) {
          createAlert(session, "ErrAlertBi", "ErrAlertBiId", title = "", content = ERROR$MsgErrorBi, append = FALSE, style='danger')
       }
    })

    #----------------------------------------------------
    # Observer - Bivariate
    #----------------------------------------------------
    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          # Annotation
          fa_options <- c("None", .C(features[,2]))
          names(fa_options) <- c('---', .C(features$Description))
          updateSelectInput(session, "biAnnot", choices = fa_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 1:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          if (inDSselect != input$inDSselect) getVars(.N(input$inDSselect))
          # First Factor
          f1_options <- .C(facnames[,2])
          names(f1_options) <- .C(facnames$Description)
          updateSelectInput(session, "biFacX", choices = f1_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 2:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          # Select the variable to be analysed
          v_options <- c(0, 1:dim(varnames)[1] )
          names(v_options) <- c('---',.C(gsub(" \\(.+\\)","",varnames$Description)))
          updateSelectInput(session, "biVarSelect1", choices = v_options)
          updateSelectInput(session, "biVarSelect2", choices = v_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 3:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0 && ! is.null(input$biFacX) && nchar(input$biFacX)>0) {
           facvals <- data[ , input$biFacX]
           if (is.numeric(facvals)) {
               fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c( 1:length(levelFac) )
           names(l_options) <- c(as.character(c(levelFac)))
           updateSelectInput(session, "SelFacX2", choices = l_options, selected=l_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 4:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (! is.null(input$inDSselect) && input$inDSselect>0 && ! is.null(input$biAnnot) && nchar(input$biAnnot)>0) {
          f_options <- c(.C(input$biAnnot))
          names(f_options) <- c('---')
          if (.C(input$biAnnot) != "None") {
              fvals <- data[ , input$biAnnot]
              if (is.numeric(fvals)) {
                 fmt <- paste('%0',round(log10(max(abs(fvals)))+0.5)+3,'.2f',sep='')
                 fvals <- as.character(sprintf(fmt, fvals))
              }
              flevels <- .C( levels(as.factor(fvals)) )
              if (length(flevels)<nbopt_multiselect) {
                  f_options <- c( 1:length(flevels) )
                  names(f_options) <- c(as.character(c(flevels)))
              }
          }
          updateSelectInput(session, "biFeatures", choices = f_options, selected=f_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 5:\n", e ); }) })

    #----------------------------------------------------
    # renderUI - Bivariate : ScatterPlot
    #----------------------------------------------------
    output$ScatterPlot <- renderPlotly ({
    tryCatch({ 
        if (! is.null(input$inDSselect) && ! is.null(isolate(input$biFacX)) && ! is.null(input$SelFacX2) && 
            ! is.null(input$biVarSelect1) && ! is.null(input$biVarSelect2) && input$inDSselect>0) {
            values$launch
            F1 <- isolate(input$biFacX)
            FA <- isolate(input$biAnnot)
            FCOL <- ifelse( FA=="None", '', FA )
            selectFCOL <- input$biFeatures
            if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
                selectFCOL <- c()
            }
            if (nchar(F1)>0 && nchar(input$biVarSelect1)>0 && nchar(input$biVarSelect2)>0) {
                varX <- .C(varnames$Attribute)[.N(input$biVarSelect1)]
                varY <- .C(varnames$Attribute)[.N(input$biVarSelect2)]
                fMean <- FALSE
                withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
                    tryCatch({
                       getScatterPLot(F1, input$SelFacX2, FCOL, selectFCOL=selectFCOL, varX, varY, fMean, blabels=input$biLabels, 
                                          gAddon=input$biAddon, blog=c(input$biLogX,input$biLogY), reglin=input$gregmod)
                    }, error=function(e) {})
                })
            }
        }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("RenderPlotly:\n", e ); })
    })

    #----------------------------------------------------
    # Bivariate : ScatterPlot 
    #----------------------------------------------------
    getScatterPLot <- function(F1, selectF1, FCOL, selectFCOL, varX, varY, fMean, blabels=FALSE, gAddon="none",blog=c(FALSE,FALSE), reglin=FALSE) {

        # Factor levels
        facvals <- data[ , F1]
        if (is.numeric(facvals)) {
            fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
            facvals <- as.character(sprintf(fmt, facvals))
        }
        levelFac <- .C( levels(as.factor(facvals)) )

        # Features as annotation
        fannot=TRUE
        if (is.null(FCOL) || nchar(FCOL)==0) { FCOL <- F1; fannot=FALSE; }
        FCOL <- tryCatch( { if(length(data[, FCOL ])) FCOL  }, error=function(e) { F1 })
        cfacvals <- as.vector(data[ , FCOL])
        cfacvals[is.na(cfacvals)] <- "NA"
        if (is.numeric(cfacvals)) {
            fmt <- paste('%0',round(log10(max(abs(cfacvals)))+0.5)+3,'.2f',sep='')
            cfacvals <- as.character(sprintf(fmt, cfacvals))
        }
        levelcFac <- .C( levels(as.factor(cfacvals)) )

        # Data extraction
        subdata <- cbind( data[ , c( varX, varY, samples) ], facvals, cfacvals )
        colnames(subdata) <- c ( varX, varY, samples, F1, FCOL )
        if (! is.null(selectF1)) {
            subdata <- subdata[subdata[ , F1 ] %in% levelFac[.N(selectF1)], ]
            if (fannot && length(selectFCOL)>0)
                subdata <- subdata[subdata[ , FCOL ] %in% levelcFac[.N(selectFCOL)], ]
        }

        # Deal with NA
        dat <- NULL
        subS <- S[ S %in% sort(subdata[ ,samples ]) ]
        for (si in 1:length(subS)) {
           D <- subdata[subdata[, samples] == subS[si],]
           if ( (length(D[, varX]) - sum(is.na(D[ ,varX])))/length(D[, varX]) <0.5 ) next
           if ( (length(D[, varY]) - sum(is.na(D[ ,varY])))/length(D[, varY]) <0.5 ) next
           if (fMean) {
               Mx <- mean(D[, varX], na.rm=T)
               My <- mean(D[, varY], na.rm=T)
               D[1, varX] <- Mx
               D[1, varY] <- My
               dat <- rbind(dat,D[1,])
           } else {
               D[is.na(D[, varX]), varX] <- mean(D[, varX], na.rm=T)
               D[is.na(D[, varY]), varY] <- mean(D[, varY], na.rm=T)
               dat <- rbind(dat,D)
           }
        }

        # Select type of IDS as labels
        xvar=.N(dat[, varX]); if (blog[1]) xvar <- log10( xvar + 1 );
        yvar=.N(dat[, varY]); if (blog[2]) yvar <- log10( yvar + 1 ); 
        if (fannot==TRUE) {
           dfg <- data.frame(factor1=as.factor(dat[,F1]), xvar=xvar, yvar=yvar, IDS=dat[, FCOL])
        } else {
           dfg <- data.frame(factor1=as.factor(dat[,F1]), xvar=xvar, yvar=yvar, IDS=dat[, samples])
        }
        dfg <- unique(dfg)

        xname  <- as.character(LABELS[LABELS[,1]==varX,2])
        if (blog[1]) xname  <- paste("Log10[",xname,"]")
        yname  <- as.character(LABELS[LABELS[,1]==varY,2])
        if (blog[2]) yname  <- paste("Log10[",yname,"]")
        F1name <- as.character(LABELS[LABELS[,1]==F1,2])

        # Calculate Ellipse
        centroids <- aggregate(cbind(xvar,yvar) ~ factor1 , dfg, mean)
        conf.rgn  <- do.call(rbind,lapply(unique(dfg$factor1),function(t)
          data.frame(factor1=as.character(t), 
                     ellipse(cov(dfg[dfg$factor1==t,2:3]), centre=as.matrix(centroids[centroids$factor1==t,2:3]), level=0.95, npoints=50), 
                     stringsAsFactors=FALSE)))
        for( i in 1:length(levels(facvals)) ) {
             conf.rgn$factor1[ conf.rgn$factor1==i ] <- levels(facvals)[i]
        }

        if (reglin) {
            fit <- lm(yvar~xvar, data=dfg)
            fit.title <- paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                              "Intercept =",signif(fit$coef[[1]],5 ),
                              " Slope =",signif(fit$coef[[2]], 5),
                              " Pvalue =",signif(summary(fit)$coef[2,4], 5))
        }

        # plot
        SE <- FALSE
        sizeP <- ifelse( blabels, 0, 0 )
        if (gAddon=="smooth")  {
            G1 <- ggplot(aes(x=xvar, y=yvar), data = dfg)
            if (gAddon=="smooth") G1 <- G1 + stat_smooth()
        } else {
            G1 <- ggplot(aes(x=xvar, y=yvar, colour=factor1), data = dfg)
            if (gAddon=="regmod")  G1 <- G1 + stat_smooth(method=lm, size=1, se = SE )
            if (gAddon=="ellipse") G1 <- G1 + geom_path(data=conf.rgn)
        }
        if (!blabels) G1 <- G1 + geom_point(aes(colour=factor1), size=sizeP)
        if (blabels) G1 <- G1 + geom_text(aes(label=IDS, colour=factor1), hjust=0.5, vjust=0.5, size=4)
        if (reglin) G1 <- G1 + stat_smooth(data = dfg, method = "lm", col = "red", size=1, se=SE) + ggtitle(fit.title)
        G1 <- G1 + labs(x=xname, y=yname, colour=F1name)
        G1 <- G1 + theme(plot.title = element_text(size=4, lineheight=.4, face="bold"))
        G1 <- G1 + theme_bw()
        ggplotly(G1)
        #G1
    }
