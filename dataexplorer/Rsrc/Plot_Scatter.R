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
       if ( values$launch>0 ) {
          # First Factor
          f1_options <- .C(g$facnames[,2])
          names(f1_options) <- .C(g$facnames$Description)
          selfac <- f1_options[1]
          if (nchar(ui$fac1)>0 && ui$fac1 %in% .C(g$facnames[,3]))
              selfac <- f1_options[which(g$facnames[,3] %in% ui$fac1)]
          updateSelectInput(session, "biFacX", choices = f1_options, selected=selfac)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 1a:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 && ! is.null(input$biFacX) && nchar(input$biFacX)>0) {
           facvals <- g$data[ , input$biFacX]
           if (is.numeric(facvals) && sum(facvals-floor(facvals))>0) {
               fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c('')
           names(l_options) <- c('---')
           if (length(levelFac)<gv$nbopt_multiselect) {
              l_options <- c( 1:length(levelFac) )
              names(l_options) <- c(as.character(c(levelFac)))
           }
           updateSelectInput(session, "SelFacX2", choices = l_options, selected=l_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 1b:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       input$IdMenu
       if ( input$IdMenu=="bivariate" && values$launch>0) {
          # Select the variable to be analysed
          v_options <- c()
          if ("data.frame" %in% class(g$varnames)) {
             v_options <- c(0, 1:nrow(g$varnames) )
             names(v_options) <- c('---',.C(gsub(" \\(.+\\)","",g$varnames$Description)))
          }
          selVar1 <- NULL
          if (nchar(ui$var1)>0 && ui$var1 %in% .C(g$varnames[,3]))
              selVar1 <- which(.C(g$varnames[,3]) %in% ui$var1)
          selVar2 <- NULL
          if (nchar(ui$var2)>0 && ui$var2 %in% .C(g$varnames[,3]))
              selVar2 <- which(.C(g$varnames[,3]) %in% ui$var2)
          updateSelectInput(session, "biVarSelect1", choices = v_options, selected=selVar1)
          updateSelectInput(session, "biVarSelect2", choices = v_options, selected=selVar2)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 2:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 ) {
          # Annotation
          fa_options <- c("None", .C(g$features[,2]))
          names(fa_options) <- c('---', .C(g$features$Description))
          updateSelectInput(session, "biAnnot", choices = fa_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 3a:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (values$launch>0 && ! is.null(input$biAnnot) && nchar(input$biAnnot)>0) {
          f_options <- c(.C(input$biAnnot))
          names(f_options) <- c('---')
          if (.C(input$biAnnot) != "None") {
              fvals <- g$data[ , input$biAnnot]
              fident <- ifelse( input$biAnnot %in% g$identifiers$Attribute, TRUE, FALSE)
              if (! fident && is.numeric(fvals) && sum(fvals-floor(fvals))>0) {
                 fmt <- paste('%0',round(log10(max(abs(fvals)))+0.5)+3,'.2f',sep='')
                 fvals <- as.character(sprintf(fmt, fvals))
              }
              flevels <- levels(as.factor(fvals))
              if (length(flevels)<gv$nbopt_multiselect) {
                  f_options <- c( 1:length(flevels) )
                  names(f_options) <- c(as.character(c(flevels)))
              }
          }
          updateSelectInput(session, "biFeatures", choices = f_options, selected=f_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 3b:\n", e ); }) })

    #----------------------------------------------------
    # renderUI - Bivariate : ScatterPlot
    #----------------------------------------------------
    output$ScatterPlot <- renderPlotly ({
    tryCatch({ 
        if ( values$launch>0 && ! is.null(isolate(input$biFacX)) && ! is.null(input$SelFacX2) && 
            ! is.null(input$biVarSelect1) && ! is.null(input$biVarSelect2)) {
            F1 <- isolate(input$biFacX)
            FA <- isolate(input$biAnnot)
            FCOL <- ifelse( FA=="None", '', FA )
            selectFCOL <- input$biFeatures
            if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
                selectFCOL <- c()
            }
            if (nchar(F1)>0 && nchar(input$biVarSelect1)>0 && nchar(input$biVarSelect2)>0) {
                varX <- .C(g$varnames$Attribute)[.N(input$biVarSelect1)]
                varY <- .C(g$varnames$Attribute)[.N(input$biVarSelect2)]
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
    getScatterPLot <- function(F1, selectF1, FCOL, selectFCOL, varX, varY, fMean, blabels=FALSE, gAddon="none",blog=c(FALSE,FALSE), reglin=FALSE)
    {
        # Factor levels
        facvals <- g$data[ , F1]
        if (is.numeric(facvals) && sum(facvals-floor(facvals))>0) {
            fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
            facvals <- as.character(sprintf(fmt, facvals))
        }
        levelFac <- .C( levels(as.factor(facvals)) )

        # Features as annotation
        fannot=TRUE
        if (is.null(FCOL) || nchar(FCOL)==0) { FCOL <- F1; fannot=FALSE; }
        FCOL <- tryCatch( { if(length(g$data[, FCOL ])) FCOL  }, error=function(e) { F1 })
        cfacvals <- as.vector(g$data[ , FCOL])
        ofacvals <- order(cfacvals)
        cfacvals[is.na(cfacvals)] <- "NA"
        fident <- ifelse( FCOL %in% g$identifiers$Attribute, TRUE, FALSE )
        if (! fident && is.numeric(cfacvals) && sum(cfacvals-floor(cfacvals))>0) {
            fmt <- paste('%0',round(log10(max(abs(cfacvals)))+0.5)+3,'.2f',sep='')
            cfacvals <- as.character(sprintf(fmt, cfacvals))
        }

        # Data extraction
        subdata <- cbind( g$data[ , c( varX, varY, g$samples) ], facvals, cfacvals )
        if (fannot && length(selectFCOL)>0)
            subFCOL <- unique(subdata$cfacvals[ofacvals])[.N(selectFCOL)]

        colnames(subdata) <- c ( varX, varY, g$samples, F1, FCOL )
        if (! is.null(selectF1)) {
            subdata <- subdata[subdata[ , F1 ] %in% levelFac[.N(selectF1)], ]
            if (fannot && length(selectFCOL)>0)
                subdata <- subdata[subdata[ , FCOL ] %in% subFCOL, ]
        }

        # Deal with NA only if less than 2000 for faster computation
        if (nrow(subdata)<2000) {
           dat <- NULL
           subS <- g$S[ g$S %in% sort(subdata[ , g$samples ]) ]
           for (si in 1:length(subS)) {
              D <- subdata[subdata[, g$samples] == subS[si],]
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
        } else {
           T1 <- subdata[!is.na(subdata[,varX]), ]
           dat <- T1[!is.na(T1[,varY]), ]
        }

        # Select type of IDS as labels
        xvar=.N(dat[, varX]); if (blog[1]) xvar <- log10( xvar + 1 );
        yvar=.N(dat[, varY]); if (blog[2]) yvar <- log10( yvar + 1 ); 
        if (fannot==TRUE) {
           dfg <- data.frame(factor1=as.factor(dat[,F1]), xvar=xvar, yvar=yvar, IDS=dat[, FCOL])
        } else {
           dfg <- data.frame(factor1=as.factor(dat[,F1]), xvar=xvar, yvar=yvar, IDS=dat[, g$samples])
        }
        dfg <- unique(dfg)

        xname  <- as.character(g$LABELS[g$LABELS[,2]==varX,4])
        if (blog[1]) xname  <- paste("Log10[",xname,"]")
        yname  <- as.character(g$LABELS[g$LABELS[,2]==varY,4])
        if (blog[2]) yname  <- paste("Log10[",yname,"]")
        F1name <- as.character(g$LABELS[g$LABELS[,2]==F1,4])

        # Calculate Linear Model
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
            if (gAddon=="ellipse") G1 <- G1 + stat_ellipse(type='norm', level=0.95, na.rm=TRUE)
        }
        if (!blabels) G1 <- G1 + geom_point(aes(colour=factor1), size=sizeP)
        if (blabels) G1 <- G1 + geom_text(aes(label=IDS, colour=factor1), hjust=0.5, vjust=0.5, size=4)
        if (reglin) G1 <- G1 + stat_smooth(data = dfg, method = "lm", col = "red", size=1, se=SE) + ggtitle(fit.title)
        G1 <- G1 + labs(x=xname, y=yname, colour=F1name)
        G1 <- G1 + theme(plot.title = element_text(size=4, lineheight=.4, face="bold"))
        G1 <- G1 + theme_bw()
        #ggplotly(G1)
        G1
    }
