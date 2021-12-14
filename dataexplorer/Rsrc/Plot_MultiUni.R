
    #----------------------------------------------------
    # Observer - ERROR
    #----------------------------------------------------
    observeEvent( ERROR$MsgErrorMuni, {
       if (nchar(ERROR$MsgErrorMuni)>0) {
          createAlert(session, "ErrAlertMuni", "ErrAlertMuniId", title = "", content = ERROR$MsgErrorMuni, append = FALSE, style='danger')
       }
    })

    #----------------------------------------------------
    # Observer - Multi-Univariate
    #----------------------------------------------------
    observeEvent( list(input$inDselect, values$launch), { tryCatch({
       if ( values$launch>0 ) {
          # First Factor
          f1_options <- .C(g$facnames[,2])
          names(f1_options) <- .C(g$facnames$Description)
          selfac1 <- f1_options[1]
          if (nchar(ui$fac1)>0 && ui$fac1 %in% .C(g$facnames[,3]))
              selfac1 <- f1_options[which(g$facnames[,3] %in% ui$fac1)]
          updateSelectInput(session, "muniFacX", choices = f1_options, selected=selfac1)
       }
    }, error=function(e) { ERROR$MsgErrorMuni <- paste("Observer 1a:\n", e ); }) })

    observeEvent( list(input$inDselect, values$launch, input$muniFacX), { tryCatch({
       if (values$launch>0 && 
           ! is.null(input$muniFacX) && nchar(input$muniFacX)>0 && input$muniFacX %in% colnames(g$data) ) {
           facvals <- g$data[ , input$muniFacX]
           if (is.numeric(facvals) && sum(facvals-floor(facvals))>0) {
               fmt <- paste('%0',round(log10(max(abs(facvals)))+0.5)+3,'.2f',sep='')
               facvals <- as.character(sprintf(fmt, facvals))
           }
           levelFac <- .C( levels(as.factor(facvals)) )
           l_options <- c(0)
           names(l_options) <- c('---')
           if (length(levelFac)<gv$nbopt_multiselect) {
              l_options <- c( l_options, 1:length(levelFac) )
              names(l_options) <- c('---',as.character(c(levelFac)))
           }
           sellev1 <- "0"
           if (nchar(ui$lev1)>0 && ui$lev1 %in% levelFac) sellev1 <- as.character(which(levelFac %in% ui$lev1))
           updateSelectInput(session, "SelLev1", choices = l_options, selected=sellev1)
           sellev2 <- "0"
           if (nchar(ui$lev2)>0 && ui$lev2 %in% levelFac) sellev2 <- as.character(which(levelFac %in% ui$lev2))
           updateSelectInput(session, "SelLev2", choices = l_options, selected=sellev2)
       }
    }, error=function(e) { ERROR$MsgErrorMuni <- paste("Observer 1b:\n", e ); }) })


    toggleElems <- function(flg) {
        elements <- c('muniFacX', 'SelLev1','SelLev2','ptype','tadj')
        for (elem in elements)
           if (flg==0) shinyjs::disable(elem) else shinyjs::enable(elem)
    }

    toggleElems2 <- function(flg) {
        elements <- c('pval', 'foldChange','onlytop')
        for (elem in elements)
           if (flg==0) shinyjs::disable(elem) else shinyjs::enable(elem)
    }

    #----------------------------------------------------
    # Compute the volcano data
    #----------------------------------------------------

    volcanoData <- eventReactive(list(input$SelLev1, input$SelLev2, input$ptype, input$tadj),
    {
       dat <- NULL
       tryCatch({ repeat {
         facname <- input$muniFacX
         faclevref <- .N(input$SelLev1)
         faclevcomp <- .N(input$SelLev2)
         ptype <- .N(input$ptype)
         tadj <- input$tadj
         if (faclevref==0 || faclevcomp==0 || faclevcomp==faclevref) break

         toggleElems(0)

         varnames <- g$varnames$Attribute
         varlabels <- gsub(" \\(.+\\)", "", g$varnames$Description)
         faclevs <- unique( sort(g$data[ , facname ]) )
         nblevs <- length(faclevs)
         data <- unique( g$data[ , c(facname,varnames) ] )
         datafac <- data[ , facname ]
         datavar <- data[ , varnames ]

         # Split the matrix by group
         new_mats<-c()
         for (i in 1:2)
            new_mats[i]<-list( datavar[which(datafac==faclevs[c(faclevref, faclevcomp)][i]),] )

         #  Calculate the means
         means <- t(simplify2array( lapply(1:length(new_mats), function(x) { apply(new_mats[[x]],2,mean,na.rm=TRUE) }) ))
         dimnames(means) <- list(faclevs[c(faclevref, faclevcomp)],colnames(datavar))

         #  Calculate the fold change
         folds <- t(apply(means, 1, function(x) { x/means[1,] } ))
         dimnames(folds) <- list(rownames(means),colnames(means))

         # Perform the comparison of means (T-test / Wilcoxon)
         cl <- makeCluster(gv$nbcores, type  ="PSOCK" )
         registerDoParallel(cl)
         pvals <- matrix(nrow=ncol(datavar), ncol=1)
         pvals[,1] <- foreach(k=1:nrow(pvals), .combine=c) %dopar% {
              if (ptype==0) {
                 v <- stats::t.test( new_mats[[1]][,k], new_mats[[2]][,k] )$p.value
              } else {
                 x <- c( new_mats[[1]][,k], new_mats[[2]][,k] )
                 g <- c( rep("A",length(new_mats[[1]][,k])), rep("B",length(new_mats[[2]][,k])) )
                 v <- stats::wilcox.test( x ~ g )$p.value
              }
              if (tadj != 'none') {
                 padj <-  stats::p.adjust(v , method = tadj)
                 v <- padj
              }
              v
         }
         stopCluster(cl); rm(cl) ; gc()

         # Build the matrix for plotting the volcano
         subsets <- names(g$varsBySubset)
         dat <- data.frame( .N(t(folds)[, 2]), .N(pvals[,1]), varnames, varlabels, rep("mid", length(varnames)), rep(subsets[1], length(varnames)) )
         colnames(dat) <- c( 'FoldChange', 'pvalue', 'Vars', 'Name', 'diffexpr', 'subsets' )
         if (length(subsets)>1) {
             for(i in 2:length(g$varsBySubset))
                 dat$subsets[ dat$Vars %in% g$varsBySubset[[i]] ] <- subsets[i]
         }

         toggleElems(1)

         break
       }}, error=function(e) { ERROR$MsgErrorMuni <- paste("Build data matrix:\n", e ); toggleElems(1) })
       dat
    })

    #----------------------------------------------------
    # renderUI - VolcanoPlot
    #----------------------------------------------------

    output$volcanoPlot <- renderPlotly ({
      closeAlert(session, "ErrAlertMuniId")
      values$launch
      tryCatch({
        if (values$launch==0) return( NULL )
        foldthres <- .N(input$foldChange)
        pvalthres <- .N(input$pval)
        onlyTop <- .N(input$onlytop)
        subsets <- .S(g$inDSselect)
        F1 <- input$muniFacX
        if (nchar(F1)==0 || nchar(isolate(input$SelLev1))==0 || nchar(isolate(input$SelLev2))==0) return( NULL )

        imgObj <- NULL
        toggleElems2(0)
        withProgress(message = 'Calculation in progress', detail = '... ', value = 0, {
           tryCatch({
             # Prepare Data
             dat <- volcanoData()
             dat$diffexpr[dat$FoldChange > foldthres] <- 'up'
             dat$diffexpr[dat$FoldChange < 1/foldthres] <- 'down'
             dat$diffexpr[dat$pvalue > pvalthres] <- "ns"
             dat$Vars[dat$pvalue > pvalthres] <- NA
             dat$Vars[dat$diffexpr == "mid"] <- NA
             dat$diffexpr <- factor(dat$diffexpr, levels=c("down", "up", "mid", "ns"), labels=c("Down", "Up", "p-values only", "NS"))
             diffexprcol <- c('brown2', 'chartreuse3', 'goldenrod1', 'darkviolet')
             linethrescol <- "darkgrey"
             # Build Plot
             p <- ggplot(data=dat, aes(x=log2(FoldChange), y=-log10(pvalue), label=Vars, name=Name, col=diffexpr), group=subsets) + 
                  geom_point(aes(shape=subsets))
             if (onlyTop>0) {
                topdat0 <- dat[ ! dat$diffexpr %in% c("ns","mid"), ]
                topdat1 <- topdat0[with(topdat0, order(FoldChange, pvalue)),][1:onlyTop,]
                topdat1 <- topdat1[ topdat1$FoldChange<1,  ]
                topdat2 <- topdat0[with(topdat0, order(1/FoldChange, pvalue)),][1:onlyTop,]
                topdat2 <- topdat2[ topdat2$FoldChange>1, ]
                topdat <- rbind(topdat1, topdat2)
                p <- p + geom_text(data=topdat, vjust = 0, nudge_y = 0.2)
             } else {
                p <- p + geom_text(vjust = 0, nudge_y = 0.2)
             }
             p <- p + geom_vline(xintercept=c(log2(1/foldthres),log2(foldthres)), col=linethrescol, linetype = "twodash") + 
                  geom_hline(yintercept=-log10(pvalthres), col=linethrescol, linetype = "twodash") +
                  scale_color_manual(values=diffexprcol) +
                  theme_minimal() + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

             imgObj <- p
           }, error=function(e) {})
        })
        toggleElems2(1)
        imgObj

      }, error=function(e) { ERROR$MsgErrorMuni <- paste("Render VolcanoPlot:\n", e ); toggleElems2(1) })
    })
