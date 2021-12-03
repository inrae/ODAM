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
       if ( values$launch>0 ) {
          # First Factor
          f1_options <- .C(g$facnames[,2])
          names(f1_options) <- .C(g$facnames$Description)
          selfac1 <- f1_options[1]
          if (nchar(ui$fac1)>0 && ui$fac1 %in% .C(g$facnames[,3]))
              selfac1 <- f1_options[which(g$facnames[,3] %in% ui$fac1)]
          updateSelectInput(session, "uniFacX", choices = f1_options, selected=selfac1)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 1a:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (values$launch>0 && 
           ! is.null(input$uniFacX) && nchar(input$uniFacX)>0 && input$uniFacX %in% colnames(g$data) ) {
           facvals <- g$data[ , input$uniFacX]
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
           updateSelectInput(session, "SelFacX", choices = l_options, selected=l_options)
           # Second Factor
           if (nchar(ui$fac2)==0) {
              f2_options <- .C(g$facnames[,2])
              names(f2_options) <- .C(g$facnames$Description)
              updateSelectInput(session, "uniFacY", choices = f2_options, selected=input$uniFacX)
           }
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 1b:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 ) {
          # Second Factor
          f2_options <- .C(g$facnames[,2])
          names(f2_options) <- .C(g$facnames$Description)
          selfac2 <- f2_options[1]
          if (nchar(ui$fac2)>0 && ui$fac2 %in% .C(g$facnames[,3]))
              selfac2 <- f2_options[which(g$facnames[,3] %in% ui$fac2)]
          updateSelectInput(session, "uniFacY", choices = f2_options, selected=selfac2)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 2a:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (values$launch>0 && 
           ! is.null(input$uniFacY) && nchar(input$uniFacY)>0 && input$uniFacY %in% colnames(g$data)) {
           if (input$uniFacY == input$uniFacX) {
              l_options <- c('')
           } else {
              facvals <- g$data[ , input$uniFacY]
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
           }
           updateSelectInput(session, "SelFacY", choices = l_options, selected=l_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 2b:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 ) {
          if (nrow(g$varnames)>maxVariables) return(NULL)
          v_options <- c(0, 1:nrow(g$varnames) )
          names(v_options) <- c('---',.C(gsub(" \\(.+\\)","",g$varnames$Description)))
          selVar <- NULL
          if (nchar(ui$var1)>0 && ui$var1 %in% .C(g$varnames[,3]))
              selVar <- which(.C(g$varnames[,3]) %in% ui$var1)
          updateSelectInput(session, "uniVarSelect", choices = v_options, selected=selVar)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 3:\n", e ); }) })


    observe({ tryCatch({
       input$inDselect
       if ( values$launch>0 ) {
          # Annotation
          fa_options <- c("None", .C(g$features[,2]))
          names(fa_options) <- c('---', .C(g$features$Description))
          updateSelectInput(session, "uniAnnot", choices = fa_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 4a:\n", e ); }) })

    observe({ tryCatch({
       input$inDselect
       if (values$launch>0 && ! is.null(input$uniAnnot) && nchar(input$uniAnnot)>0) {
          f_options <- c(.C(input$uniAnnot))
          names(f_options) <- c('---')
          if (.C(input$uniAnnot) != "None") {
              fvals <- g$data[ , input$uniAnnot]
              fident <- ifelse( input$uniAnnot %in% g$identifiers$Attribute, TRUE, FALSE)
              if (! fident && is.numeric(fvals) && sum(fvals-floor(fvals))>0) {
                 fmt <- paste('%0',round(log10(max(abs(fvals)))+0.5)+3,'.2f',sep='')
                 fvals <- as.character(sprintf(fmt, fvals))
              }
              flevels <- levels(as.factor(fvals))
              if (length(flevels)<nbopt_multiselect) {
                  f_options <- c( 1:length(flevels) )
                  names(f_options) <- c(as.character(c(flevels)))
              }
          }
          updateSelectInput(session, "uniFeatures", choices = f_options, selected=f_options)
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 4b:\n", e ); }) })


    observe({ tryCatch({
       if (!is.null(input$ttest) && .N(input$ttest)==1) {
          updateCheckboxInput(session, "violin", value = FALSE)
          values$ttest <- 1
       } else {
          values$ttest <- 0
       }
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 5a:\n", e ); }) })

    observe({ tryCatch({
       if ((!is.null(input$violin) && .N(input$violin)==1) || (! .C(input$uniFacY) %in% .C(input$uniFacX)))
          updateCheckboxInput(session, "ttest", value = FALSE)
    }, error=function(e) { ERROR$MsgErrorUni <- paste("Observer 5b:\n", e ); }) })

    #----------------------------------------------------
    # renderUI - Univariate : BoxPlot + T.test
    #----------------------------------------------------
    # Render T.test Plot
    output$TtestPlot <- renderImage ({
      values$launch
      tryCatch({
        if (values$launch==0 || values$ttest==0) return( imgNull )
        F1 <- isolate(input$uniFacX)
        F2 <- isolate(input$uniFacY)
        if (F1 != F2) return( imgNull )
        SelFacX <- input$SelFacX
        SelFacY <- input$SelFacY
        FA <- isolate(input$uniAnnot)
        FCOL <- ifelse( FA=="None", '', FA )
        selectFCOL <- input$uniFeatures
        if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
            selectFCOL <- c()
        }
        if (nchar(F1)>0 &&  nchar(F2)>0 && nchar(input$uniVarSelect)>0 ) {
            varX <- .C(g$varnames$Attribute)[.N(input$uniVarSelect)]
            if (ui$header %in% c('off')) runjs('$(".content-wrapper").css("min-height", "0px");')
            withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
               tryCatch({
                  imgObj <- getBoxPLot1(F1, SelFacX, FCOL, selectFCOL, varX, 
                              bsmooth=input$uniSmooth, blog=input$uniLog, bviolin=input$violin, bTtest=TRUE)
               }, error=function(e) {})
            })
            imgObj
        }
      }, error=function(e) { ERROR$MsgErrorUni <- paste("RenderImage:\n", e ); })
    })

    #----------------------------------------------------
    # renderUI - Univariate : BoxPlot
    #----------------------------------------------------
    output$BoxPlot <- renderPlotly ({
      values$launch
      tryCatch({
        if (values$launch==0 || values$ttest==1) return( NULL )
        SelFacX <- input$SelFacX
        SelFacY <- input$SelFacY
        FA <- isolate(input$uniAnnot)
        FCOL <- ifelse( FA=="None", '', FA )
        selectFCOL <- input$uniFeatures
        if (nchar(FCOL)>0 && ( length(selectFCOL)==0 || (length(selectFCOL)==1 && selectFCOL[1]==FA) ) ) {
            selectFCOL <- c()
        }
        F1 <- isolate(input$uniFacX)
        F2 <- isolate(input$uniFacY)
        if (nchar(F1)>0 &&  nchar(F2)>0 && nchar(input$uniVarSelect)>0 ) {
            varX <- .C(g$varnames$Attribute)[.N(input$uniVarSelect)]
            if (ui$header %in% c('off')) runjs('$(".content-wrapper").css("min-height", "0px");')
            withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
               tryCatch({
                   if (F1==F2) {
                       getBoxPLot1(F1, SelFacX, FCOL, selectFCOL, varX, bsmooth=input$uniSmooth, blog=input$uniLog, bviolin=input$violin)
                   } else {
                       getBoxPLot2(F1, F2, SelFacX, SelFacY, FCOL, selectFCOL, varX, bsmooth=input$uniSmooth, blog=input$uniLog)
                   }
               }, error=function(e) {})
            })
        }
      }, error=function(e) { ERROR$MsgErrorUni <- paste("RenderPlotly:\n", e ); })
    })


    #----------------------------------------------------
    # Univariate : BoxPlot
    #----------------------------------------------------
    # One factor
    getBoxPLot1 <- function(F1, selectF1, FCOL, selectFCOL, varX, bsmooth=FALSE, blog=FALSE, bviolin=FALSE, bTtest=FALSE) {
        # Factor levels F1
        facval1 <- g$data[ , F1]
        if (is.numeric(facval1)  && sum(facval1-floor(facval1))>0) {
            fmt <- paste('%0',round(log10(max(abs(facval1)))+0.5)+3,'.2f',sep='')
            facval1 <- as.character(sprintf(fmt, facval1))
        }
        levelFac1 <- .C( levels(as.factor(facval1)) )

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
        subdata <- cbind( g$data[ , c( varX, g$samples) ], facval1, cfacvals)
        if (fannot && length(selectFCOL)>0)
            subFCOL <- unique(subdata$cfacvals[ofacvals])[.N(selectFCOL)]

        colnames(subdata) <- c ( varX, g$samples, F1, FCOL )
        if (! is.null(selectF1)) {
            subdata <- subdata[subdata[ , F1 ] %in% levelFac1[.N(selectF1)], ]
            if (fannot && length(selectFCOL)>0)
                subdata <- subdata[subdata[ , FCOL ] %in% subFCOL, ]
        }
        dat <- subdata

        colorid <- F1
        xid <- F1
        yid <- varX
        if (blog) {
            df <- data.frame( colour=as.factor(dat[,colorid]), x=as.factor(dat[,xid]), y=log10(dat[, yid]+0.01) )
        } else {
            df <- data.frame( colour=as.factor(dat[,colorid]), x=as.factor(dat[,xid]), y=dat[, yid] )
        }

        xname     <- as.character(g$LABELS[g$LABELS[,2]==xid,4])
        yname     <- as.character(g$LABELS[g$LABELS[,2]==yid,4])
        if (blog) yname  <- paste("Log10[",yname,"]")
        colorname <- as.character(g$LABELS[g$LABELS[,2]==colorid,4])

        if (bTtest) {
           # Factor levels
           V <- levels(df$colour)
           
           # Set of pairs of two levels
           L <- list();  for (i in 1:(length(V)-1)) L[[length(L)+1]] <- c(V[i], V[i+1]); L[[length(L)+1]] <- c(V[1], V[length(V)]); 
           
           # Calculation of the ordinates for the annotation of the p-values
           ylabs.v <- NULL
           delta <- max(df$y, na.rm=TRUE) -  min(df$y, na.rm=TRUE)
           for (i in 1:(length(L)-1))
               ylabs.v <- c(ylabs.v, 1.1*quantile(df[ df$colour %in% L[[i]], ]$y,  probs = seq(0, 1, 0.15), na.rm = TRUE)[7])
           ylabs.v <- c(ylabs.v, 0.1*delta + max(ylabs.v))

           # Boxplot with p-values of comparisons
           G1 <- ggboxplot(df, x = "x", y = "y", color = "colour") +
                stat_compare_means(comparisons = L, label.y = ylabs.v)
           # Smoothed curve (loess)
           G1 <- G1 + geom_smooth(aes(group = 1), span=0.75, method="loess", size=2, se = FALSE )
           # Theme and legend settings
           G1 <- ggpar(G1, ggtheme=theme_bw(), legend="right", xlab = xname, ylab = yname, legend.title=colorname)

           # Save graphic as SVG
           width  <- session$clientData$output_TtestPlot_width
           height <- session$clientData$output_TtestPlot_height
           mysvgwidth <- width/90
           mysvgheight <- height/90

           outfile <- file.path(SESSTMPDIR,outfiles[['TTEST']])
           ggsave(file=outfile, plot=G1, width=mysvgwidth, height=mysvgheight)

           list(src = outfile,
              contentType = 'image/svg+xml',
              width = width,
              height = height,
              alt = ' SVG Image'
           )

        } else {
           # plot
           G1 <- ggplot(aes(y=y, x=x, colour=colour), data = df, family="Times", lineheight=.8, fontface="bold")
           if (bviolin) G1 <- G1 + geom_violin()
           if (!bviolin) G1 <- G1 + geom_boxplot()
           if (bsmooth) G1 <- G1 + geom_smooth(aes(group = 1), span=0.75, method="loess", size=2, se = FALSE )
           G1 <- G1 + labs(x=xname, y=yname, colour=colorname)
           G1 <- G1 + theme(plot.title = element_text(size=12, lineheight=.8, face="bold"), 
                            axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))
           G1 <- G1 + theme_bw()
           #ggplotly(G1)
           G1
        }
    }

    # Two factors
    getBoxPLot2 <- function(F1, F2, selectF1, selectF2, FCOL, selectFCOL, varX, bsmooth=FALSE, blog=FALSE, bviolin=FALSE) {

        # Factor levels F1
        facval1 <- g$data[ , F1]
        if (is.numeric(facval1) && sum(facval1-floor(facval1))>0) {
            fmt <- paste('%0',round(log10(max(abs(facval1)))+0.5)+3,'.2f',sep='')
            facval1 <- as.character(sprintf(fmt, facval1))
        }
        levelFac1 <- .C( levels(as.factor(facval1)) )

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

        # Factor levels F2
        facval2 <- g$data[ , F2]
        if (is.numeric(facval2) && sum(facval2-floor(facval2))>0) {
            fmt <- paste('%0',round(log10(max(abs(facval2)))+0.5)+3,'.2f',sep='')
            facval2 <- as.character(sprintf(fmt, facval2))
        }
        levelFac2 <- .C( levels(as.factor(facval2)) )

        # Data extraction
        subdata <- cbind( g$data[ , c( varX, g$samples) ], facval1, facval2, cfacvals )
        if (fannot && length(selectFCOL)>0)
            subFCOL <- unique(subdata$cfacvals[ofacvals])[.N(selectFCOL)]

        colnames(subdata) <- c ( varX, g$samples, F1, F2, FCOL )
        if (! is.null(selectF1))
            subdata <- subdata[subdata[ , F1 ] %in% levelFac1[.N(selectF1)], ]
        if (! is.null(selectF2))
            subdata <- subdata[subdata[ , F2 ] %in% levelFac2[.N(selectF2)], ]
        if ((! is.null(selectF1) || ! is.null(selectF2)) && fannot && length(selectFCOL)>0)
            subdata <- subdata[subdata[ , FCOL ] %in% subFCOL, ]

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

        xname     <- as.character(g$LABELS[g$LABELS[,2]==xid,4])
        yname     <- as.character(g$LABELS[g$LABELS[,2]==yid,4])
        if (blog) yname  <- paste("Log10[",yname,"]")
        colorname <- as.character(g$LABELS[g$LABELS[,2]==colorid,4])

        # plot
        G2 <- ggplot(aes(y=y, x=x, colour=colour), data = df, family="Times", lineheight=.8, fontface="bold")
        if (bviolin) G2 <- G2 + geom_violin()
        if (!bviolin) G2 <- G2 + geom_boxplot()
        G2 <- G2 + labs(x=xname, y=yname, colour=colorname)
        G2 <- G2 + theme(plot.title = element_text(size=12, lineheight=.8, face="bold"))
        G2 <- G2 + theme_bw()
        if (bsmooth) G2 <- G2 + stat_smooth(aes(group=colour), size=2, se = FALSE )
        #ggplotly(G2)
        G2
    }
