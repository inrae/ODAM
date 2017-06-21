    #----------------------------------------------------
    # Univariate : BoxPlot
    #----------------------------------------------------
    # One factor
    getBoxPLot1 <- function(F1, selectF1, varX, fMean, bsmooth=FALSE, blog=FALSE) {
        # Factor levels F1
        facval1 <- data[ , F1]
        if (is.numeric(facval1)) {
            fmt <- paste('%0',round(log10(max(abs(facval1))))+3,'.2f',sep='')
            facval1 <- as.character(sprintf(fmt, facval1))
        }
        levelFac1 <- .C( levels(as.factor(facval1)) )

        # Data extraction
        subdata <- cbind( data[ , c( varX, samples) ], facval1 )
        colnames(subdata) <- c ( varX, samples, F1 )
        if (! is.null(selectF1)) {
            subdata <- subdata[subdata[ , F1 ] %in% levelFac1[.N(selectF1)], ]
        }

        # Deal with NA
        dat <- NULL
        subS <- S[ S %in% sort(subdata[ ,samples ]) ]
        for (si in 1:length(subS)) {
           D <- subdata[subdata[, samples] == subS[si],]
           if ( (length(D[, varX]) - sum(is.na(D[ ,varX])))/length(D[, varX]) <0.5 ) next
           if (fMean) {
               Mx <- mean(D[, varX], na.rm=T)
               D[1, varX] <- Mx
               dat <- rbind(dat,D[1,])
           } else {
               if (length(D[is.na(D[, varX]), varX])>0) {
                   D[is.na(D[, varX]), varX] <- mean(D[, varX], na.rm=T)
               }
               dat <- rbind(dat,D)
           }
        }

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
        G2 <- ggplot(aes(y=y, x=x, colour=colour), data = df, family="Times", lineheight=.8, fontface="bold") + geom_boxplot()
        if (bsmooth) G2 <- G2 + geom_smooth(aes(group = 1), span=0.75, method="loess", size=2, se = FALSE )
        G2 <- G2 + labs(x=xname, y=yname, colour=colorname)
        G2 <- G2 + theme(plot.title = element_text(size=12, lineheight=.8, face="bold"))
        #gg <- ggplotly(G2)
        #gg
        G2
    }

    # Two factors
    getBoxPLot2 <- function(F1, F2, selectF1, selectF2, varX, fMean, bsmooth=FALSE, blog=FALSE) {

        # Factor levels F1
        facval1 <- data[ , F1]
        if (is.numeric(facval1)) {
            fmt <- paste('%0',round(log10(max(abs(facval1))))+3,'.2f',sep='')
            facval1 <- as.character(sprintf(fmt, facval1))
        }
        levelFac1 <- .C( levels(as.factor(facval1)) )

        # Factor levels F2
        facval2 <- data[ , F2]
        if (is.numeric(facval2)) {
            fmt <- paste('%0',round(log10(max(abs(facval2))))+3,'.2f',sep='')
            facval2 <- as.character(sprintf(fmt, facval2))
        }
        levelFac2 <- .C( levels(as.factor(facval2)) )

        # Data extraction
        subdata <- cbind( data[ , c( varX, samples) ], facval1, facval2 )
        colnames(subdata) <- c ( varX, samples, F1, F2 )
        if (! is.null(selectF1)) {
            subdata <- subdata[subdata[ , F1 ] %in% levelFac1[.N(selectF1)], ]
        }
        if (! is.null(selectF2)) {
            subdata <- subdata[subdata[ , F2 ] %in% levelFac2[.N(selectF2)], ]
        }

        # Deal with NA
        dat <- NULL
        subS <- S[ S %in% sort(subdata[ ,samples ]) ]
        for (si in 1:length(subS)) {
           D <- subdata[subdata[, samples] == subS[si],]
           if ( (length(D[, varX]) - sum(is.na(D[ ,varX])))/length(D[, varX]) <0.5 ) next
           if (fMean) {
               Mx <- mean(D[, varX], na.rm=T)
               D[1, varX] <- Mx
               dat <- rbind(dat,D[1,])
           } else {
               if (length(D[is.na(D[, varX]), varX])>0) {
                   D[is.na(D[, varX]), varX] <- mean(D[, varX], na.rm=T)
               }
               dat <- rbind(dat,D)
           }
        }

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
        colorcond <- as.vector(dat[ ,colorid]);

        # plot
        G2 <- ggplot(aes(y=y, x=x, colour=colour), data = df, family="Times", lineheight=.8, fontface="bold") + geom_boxplot()
        G2 <- G2 + labs(x=xname, y=yname, colour=colorname)
        G2 <- G2 + theme(plot.title = element_text(size=12, lineheight=.8, face="bold"))
        if (bsmooth) G2 <- G2 + stat_smooth(aes(group=colour), size=2, se = FALSE )
        #gg <- ggplotly(G2)
        #gg
        G2
    }

    #----------------------------------------------------
    # Observer - Univariate
    #----------------------------------------------------
    observe({
       if ( ! is.null(input$inDSelect) && input$inDSelect>0) {
          if (inDSelect != input$inDSelect) getVars(.N(input$inDSelect))
          # First Factor
          f1_options <- .C(facnames[,2])
          names(f1_options) <- .C(facnames$Description)
          updateSelectInput(session, "uniFacX", choices = f1_options)
          # Second Factor
          f2_options <- .C(facnames[,2])
          names(f2_options) <- .C(facnames$Description)
          updateSelectInput(session, "uniFacY", choices = f2_options)
          # Select the variable to be analysed
          v_options <- c(0, 1:dim(varnames)[1] )
          names(v_options) <- c('---',.C(varnames$Description))
          updateSelectInput(session, "uniVarSelect", choices = v_options)
       }
    })
    observe({
       if (! is.null(input$inDSelect) && input$inDSelect>0 && ! is.null(input$uniFacX) && nchar(input$uniFacX)>0) {
           facvals <- data[ , input$uniFacX]
           if (is.numeric(facvals)) {
               fmt <- paste('%0',round(log10(max(abs(facvals))))+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c( 1:length(levelFac) )
           names(l_options) <- c(as.character(c(levelFac)))
           updateSelectInput(session, "SelFacX", choices = l_options, selected=paste0(names(l_options)))
       }
    })
    observe({
       if (! is.null(input$inDSelect) && input$inDSelect>0 && ! is.null(input$uniFacY) && nchar(input$uniFacY)>0) {
           facvals <- data[ , input$uniFacY]
           if (is.numeric(facvals)) {
               fmt <- paste('%0',round(log10(max(abs(facvals))))+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c( 1:length(levelFac) )
           names(l_options) <- c(as.character(c(levelFac)))
           updateSelectInput(session, "SelFacY", choices = l_options, selected=paste0(names(l_options)))
       }
    })

    #----------------------------------------------------
    # renderUI - Univariate : BoxPlot
    #----------------------------------------------------
    output$BoxPlot <- renderPlot ({
        if (input$inDSelect==0) return( NULL )
        input$SelFacX
        input$SelFacY
        F1 <- isolate(input$uniFacX)
        F2 <- isolate(input$uniFacY)
        if (nchar(F1)>0 &&  nchar(F2)>0 && nchar(input$uniVarSelect)>0 ) {
            varX <- .C(varnames$Attribute)[.N(input$uniVarSelect)]
            fMean <- FALSE
            withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
               tryCatch({
                   if (F1==F2) {
                       getBoxPLot1(F1, input$SelFacX, varX, fMean, bsmooth=input$uniSmooth, blog=input$uniLog)
                   } else {
                       getBoxPLot2(F1, F2, input$SelFacX, input$SelFacY, varX, fMean, bsmooth=input$uniSmooth, blog=input$uniLog)
                   }
               }, error=function(e) {})
            })
        }
    })
