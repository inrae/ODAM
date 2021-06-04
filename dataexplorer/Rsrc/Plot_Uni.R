    #----------------------------------------------------
    # Univariate : BoxPlot
    #----------------------------------------------------
    # One factor
    getBoxPLot1 <- function(F1, selectF1, FCOL, selectFCOL, varX, fMean, bsmooth=FALSE, blog=FALSE, bviolin=FALSE) {
        # Factor levels F1
        facval1 <- data[ , F1]
        if (is.numeric(facval1)) {
            fmt <- paste('%0',round(log10(max(abs(facval1)))+0.5)+3,'.2f',sep='')
            facval1 <- as.character(sprintf(fmt, facval1))
        }
        levelFac1 <- .C( levels(as.factor(facval1)) )

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
        subdata <- cbind( data[ , c( varX, samples) ], facval1, cfacvals)
        colnames(subdata) <- c ( varX, samples, F1, FCOL )
        if (! is.null(selectF1)) {
            subdata <- subdata[subdata[ , F1 ] %in% levelFac1[.N(selectF1)], ]
            if (fannot && length(selectFCOL)>0)
                subdata <- subdata[subdata[ , FCOL ] %in% levelcFac[.N(selectFCOL)], ]
        }

#        # Deal with NA
#        subS <- S[ S %in% sort(subdata[ ,samples ]) ]
#        dat <- NULL
#        for (si in 1:length(subS)) {
#           D <- subdata[subdata[, samples] == subS[si],]
#           V <- D[ ,varX]
#           na.V <- is.na(V)
#           na.sum <- sum(na.V)
#           if (na.sum>0) {
#             lenD <- length(V)
#             if ( (lenD - na.sum)/lenD <0.5 ) next
#             D[na.V, varX] <- mean(V, na.rm=T)
#           }
#           dat <- rbind(dat,D)
#        }
#        dat <- unique(dat)
dat <- subdata

        colorid <- F1
        xid <- F1
        yid <- varX
        if (blog) {
            df <- data.frame( colour=as.factor(dat[,colorid]), x=as.factor(dat[,xid]), y=log10(dat[, yid]+1) )
        } else {
            df <- data.frame( colour=as.factor(dat[,colorid]), x=as.factor(dat[,xid]), y=dat[, yid] )
        }

        xname     <- as.character(LABELS[LABELS[,1]==xid,2])
        yname     <- as.character(LABELS[LABELS[,1]==yid,2])
        if (blog) yname  <- paste("Log10[",yname,"]")
        colorname <- as.character(LABELS[LABELS[,1]==colorid,2])

        # plot
        G2 <- ggplot(aes(y=y, x=x, colour=colour), data = df, family="Times", lineheight=.8, fontface="bold")
        if (bviolin) G2 <- G2 + geom_violin()
        if (!bviolin) G2 <- G2 + geom_boxplot()
        if (bsmooth) G2 <- G2 + geom_smooth(aes(group = 1), span=0.75, method="loess", size=2, se = FALSE )
        G2 <- G2 + stat_compare_means(method = "anova",label.x=2)
        G2 <- G2 + labs(x=xname, y=yname, colour=colorname)
        G2 <- G2 + theme(plot.title = element_text(size=12, lineheight=.8, face="bold"), 
                         axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))
        #ggplotly(G2)
        G2
    }

    # Two factors
    getBoxPLot2 <- function(F1, F2, selectF1, selectF2, FCOL, selectFCOL, varX, fMean, bsmooth=FALSE, blog=FALSE, bviolin=FALSE) {

        # Factor levels F1
        facval1 <- data[ , F1]
        if (is.numeric(facval1)) {
            fmt <- paste('%0',round(log10(max(abs(facval1)))+0.5)+3,'.2f',sep='')
            facval1 <- as.character(sprintf(fmt, facval1))
        }
        levelFac1 <- .C( levels(as.factor(facval1)) )

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

        # Factor levels F2
        facval2 <- data[ , F2]
        if (is.numeric(facval2)) {
            fmt <- paste('%0',round(log10(max(abs(facval2)))+0.5)+3,'.2f',sep='')
            facval2 <- as.character(sprintf(fmt, facval2))
        }
        levelFac2 <- .C( levels(as.factor(facval2)) )

        # Data extraction
        subdata <- cbind( data[ , c( varX, samples) ], facval1, facval2, cfacvals )
        colnames(subdata) <- c ( varX, samples, F1, F2, FCOL )
        if (! is.null(selectF1))
            subdata <- subdata[subdata[ , F1 ] %in% levelFac1[.N(selectF1)], ]
        if (! is.null(selectF2))
            subdata <- subdata[subdata[ , F2 ] %in% levelFac2[.N(selectF2)], ]
        if ((! is.null(selectF1) || ! is.null(selectF2)) && fannot && length(selectFCOL)>0)
            subdata <- subdata[subdata[ , FCOL ] %in% levelcFac[.N(selectFCOL)], ]

#        # Deal with NA
#        dat <- NULL
#        subS <- S[ S %in% sort(subdata[ ,samples ]) ]
#        for (si in 1:length(subS)) {
#           D <- subdata[subdata[, samples] == subS[si],]
#           if ( (length(D[, varX]) - sum(is.na(D[ ,varX])))/length(D[, varX]) <0.5 ) next
#           if (fMean) {
#               Mx <- mean(D[, varX], na.rm=T)
#               D[1, varX] <- Mx
#               dat <- rbind(dat,D[1,])
#           } else {
#               if (length(D[is.na(D[, varX]), varX])>0) {
#                   D[is.na(D[, varX]), varX] <- mean(D[, varX], na.rm=T)
#               }
#               dat <- rbind(dat,D)
#           }
#        }
#        dat <- unique(dat)
dat <- subdata

        # define the summary function
        f <- function(x) {
          r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
          names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
          r
        }
        # define outlier function, beyound 5 and 95% percentiles
        o <- function(x) {
          subset(x, x < quantile(x,probs=c(0.05))[1] | quantile(x,probs=c(0.95))[1] < x)
        }

        colorid <- F2
        xid <- F1
        yid <- varX
        if (blog) {
            df <- data.frame( colour=as.factor(dat[,colorid]), x=as.factor(dat[,xid]), y=log10(dat[, yid]+1) )
        } else {
            df <- data.frame( colour=as.factor(dat[,colorid]), x=as.factor(dat[,xid]), y=dat[, yid] )
        }

        xname     <- as.character(LABELS[LABELS[,1]==xid,2])
        yname     <- as.character(LABELS[LABELS[,1]==yid,2])
        if (blog) yname  <- paste("Log10[",yname,"]")
        colorname <- as.character(LABELS[LABELS[,1]==colorid,2])

        # plot
        G2 <- ggplot(aes(y=y, x=x, colour=colour), data = df, family="Times", lineheight=.8, fontface="bold")
        if (bviolin) G2 <- G2 + geom_violin()
        if (!bviolin) G2 <- G2 + geom_boxplot()
        G2 <- G2 + labs(x=xname, y=yname, colour=colorname)
        G2 <- G2 + theme(plot.title = element_text(size=12, lineheight=.8, face="bold"))
        if (bsmooth) G2 <- G2 + stat_smooth(aes(group=colour), size=2, se = FALSE )
        #ggplotly(G2)
        G2
    }

    #----------------------------------------------------
    # Observer - ERROR
    #----------------------------------------------------
    observe ({
       ERROR$MsgErrorUni
       if (nchar(ERROR$MsgErrorUni)>0) {
          createAlert(session, "ErrAlertUni", "ErrAlertUniId", title = "", content = ERROR$MsgErrorUni, append = FALSE, style='danger')
       }
    })

    #----------------------------------------------------
    # Observer - Univariate
    #----------------------------------------------------
    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          if (inDSselect != input$inDSselect) getVars(.N(input$inDSselect))
          # First Factor
          f1_options <- .C(facnames[,2])
          names(f1_options) <- .C(facnames$Description)
          updateSelectInput(session, "uniFacX", choices = f1_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 1a:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          if (inDSselect != input$inDSselect) getVars(.N(input$inDSselect))
          # Second Factor
          f2_options <- .C(facnames[,2])
          names(f2_options) <- .C(facnames$Description)
          updateSelectInput(session, "uniFacY", choices = f2_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 1b:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          if (inDSselect != input$inDSselect) getVars(.N(input$inDSselect))
          # Select the variable to be analysed
          v_options <- c(0, 1:dim(varnames)[1] )
          names(v_options) <- c('---',.C(gsub(" \\(.+\\)","",varnames$Description)))
          updateSelectInput(session, "uniVarSelect", choices = v_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 1c:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( ! is.null(input$inDSselect) && input$inDSselect>0) {
          # Annotation
          fa_options <- c("None", .C(features[,2]))
          names(fa_options) <- c('---', .C(features$Description))
          updateSelectInput(session, "uniAnnot", choices = fa_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 1d:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (! is.null(input$inDSselect) && input$inDSselect>0 && ! is.null(input$uniAnnot) && nchar(input$uniAnnot)>0) {
          f_options <- c(.C(input$uniAnnot))
          names(f_options) <- c('---')
          if (.C(input$uniAnnot) != "None") {
              fvals <- data[ , input$uniAnnot]
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
          updateSelectInput(session, "uniFeatures", choices = f_options, selected=f_options)
       }
    }, error=function(e) { ERROR$MsgErrorBi <- paste("Observer 1e:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (! is.null(input$inDSselect) && input$inDSselect>0 && 
           ! is.null(input$uniFacX) && nchar(input$uniFacX)>0 && input$uniFacX %in% colnames(data) ) {
           facvals <- data[ , input$uniFacX]
           if (is.numeric(facvals)) {
               fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c( 1:length(levelFac) )
           names(l_options) <- c(as.character(c(levelFac)))
           updateSelectInput(session, "SelFacX", choices = l_options, selected=l_options)
           # Second Factor
           f2_options <- .C(facnames[,2])
           names(f2_options) <- .C(facnames$Description)
           updateSelectInput(session, "uniFacY", choices = f2_options, selected=input$uniFacX)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 2:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (! is.null(input$inDSselect) && input$inDSselect>0 && 
           ! is.null(input$uniFacY) && nchar(input$uniFacY)>0 && input$uniFacY %in% colnames(data) ) {
           facvals <- data[ , input$uniFacY]
           if (is.numeric(facvals)) {
               fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c( 1:length(levelFac) )
           names(l_options) <- c(as.character(c(levelFac)))
           updateSelectInput(session, "SelFacY", choices = l_options, selected=l_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 3:\n", e ); }) })

    #----------------------------------------------------
    # renderUI - Univariate : BoxPlot
    #----------------------------------------------------
    output$BoxPlot <- renderPlotly ({
    tryCatch({ 
        if (input$inDSselect==0) return( NULL )
        values$launch
        input$SelFacY
        SelFacX <- isolate(input$SelFacX)
        FA <- isolate(input$uniAnnot)
        FCOL <- ifelse( FA=="None", '', FA )
        selectFCOL <- input$uniFeatures
        if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
            selectFCOL <- c()
        }
        F1 <- isolate(input$uniFacX)
        F2 <- isolate(input$uniFacY)
        if (nchar(F1)>0 &&  nchar(F2)>0 && nchar(input$uniVarSelect)>0 ) {
            varX <- .C(varnames$Attribute)[.N(input$uniVarSelect)]
            fMean <- FALSE
            withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
               tryCatch({
                   if (F1==F2) {
                       getBoxPLot1(F1, SelFacX, FCOL, selectFCOL, varX, fMean, bsmooth=input$uniSmooth, blog=input$uniLog, bviolin=input$violin)
                   } else {
                       getBoxPLot2(F1, F2, SelFacX, input$SelFacY, FCOL, selectFCOL, varX, fMean, bsmooth=input$uniSmooth, blog=input$uniLog, bviolin=FALSE)
                   }
               }, error=function(e) {})
            })
        }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("RenderPlotly:\n", e ); })
    })
