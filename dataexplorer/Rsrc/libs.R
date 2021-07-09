library(shiny)
library(shinyjs)
library(shinydashboard)
library(jsonlite)
library(RCurl)
library(httr)
library(reshape2)
library(pcaMethods)
library(grid)
library(ellipse)
library(JADE)
library(moments)
library(scales)
library(igraph)
library(networkD3)
library(magrittr)
library(htmlwidgets)
library(htmltools)
library(ggpubr)
library(ggdendro)
library(plotly)
library(stringr)
library(FastGGM)
library(RcppParallel)

setThreadOptions(numThreads = 4) # set 4 threads for parallel computing

auth <- ''
dsname <- ''
dcname <- ''
ws <- c(internalURL, dsname, auth, externalURL, dcname, NULL, NULL, NULL)

dclist <- NULL
inDselect <- NULL
subsets <- NULL
inDSselect <- 0
subsetNames <- NULL
connectList <- NULL
msgError <- ''
dn <- NULL
fs <- 10

data <- NULL

samplename <- NULL
samples <- NULL
S <- NULL
varnames <- NULL
setnames <- NULL
facnames <- NULL
features <- NULL
LABELS <- NULL
DSL <- NULL

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
.N <- function(x) { as.numeric(as.vector(x)) }
.C <- function(x) { as.vector(x) }

httr_get <- function(ws, query, fsplit=TRUE) {
    headers <- c( 'X-Forwarded-For' = IPClient )
    if (nchar(ws[3])>0) headers <- c( headers, 'X-Api-Key' = ws[3] )
    resp <- httr::GET(paste0(ws[4], query), config = httr::config(ssl_verifypeer = FALSE), add_headers(.headers = headers))
    T <- httr::content(resp, as='text')
    if (fsplit) T <- simplify2array(strsplit(T,"\n"))
    T
}

is.DS <- function(cdata) {
    lparams <- unlist(strsplit(gsub("\\?", "", cdata[['url_search']]),  '&'))
    ret <- FALSE
    if (!is.na(pmatch('ds', lparams)) || !is.na(pmatch('dc', lparams))) ret <- TRUE
    return(ret)
}

getME <- function(cdata) {
    protocol <- cdata[['url_protocol']]
    hostname <- cdata[['url_hostname']]
    port <- cdata[['url_port']]
    pathname <- cdata[['url_pathname']]
    paste(protocol,'//',hostname,':',port,pathname, sep='')
}

getWS <- function(cdata) {
    params <- parseQueryString(cdata$url_search)
    if (!is.null(params[['ws']])) {
        externalURL <<- params[['ws']]
    }
    dcname <- ''
    if (!is.null(params[['dc']])) {
        dcname <- params[['dc']]
    }
    dsname <- ''
    if (!is.null(params[['ds']])) {
        dsname <- params[['ds']]
    }
    auth <- ''
    if (!is.null(params[['auth']])) {
        auth <- params[['auth']]
    }
    subsetname <- NULL
    if (!is.null(params[['subset']])) {
        subsetname <- params[['subset']]
    }
    tabname <- NULL
    if (!is.null(params[['tab']])) {
        tabname <- params[['tab']]
    }
    headerflag <- NULL
    if (!is.null(params[['frame']])) {
        headerflag <- params[['frame']]
    }
    c(  internalURL, dsname, auth, externalURL, dcname, subsetname, tabname, headerflag )
}

getAbout <- function () {
    aboutfile <- '/srv/shiny-server/www/about.md'
    if(file.exists(aboutfile)){
       gsub('@@IMAGE@@/', '', readLines(aboutfile, n = -1) )
    }
}

getInfos <- function (ws) {
    str_auth <- ifelse( nchar(ws[3])>0, paste0("?auth=", ws[3]), paste0("?xff=", base64_enc(IPClient)) )
    T <- httr_get(ws, paste0('infos/', ws[2]))
    # Images
    P <- na.omit(str_extract(T, pattern="@@IMAGE@@/[^\\.]+\\.(png|jpg)"))
    if (length(P)>0)
       for (i in 1:length(P))
            T <- gsub(P[i], paste0(gsub("@@IMAGE@@", paste0(ws[4],'/image/', ws[2]), P[i] ), str_auth ), T )
    # PDF
    P <- na.omit(str_extract(T, pattern="@@PDF@@/[^\\.]+\\.pdf"))
    if (length(P)>0)
       for (i in 1:length(P))
            T <- gsub(P[i], paste0(gsub("@@PDF@@", paste0(ws[4],'/pdf/', ws[2]), P[i] ), str_auth ), T )
    T
}

getData <- function (ws, query='', dcol=0) {
    ds <- ifelse(dcol>0, ws[5], ws[2])
    out <- read.csv(textConnection(httr_get(ws, paste0('tsv/',ds,'/',query))), head=TRUE, sep="\t")
    msgError <<- ''
    if (dim(out)[1]==0) {
        msgError <<- gsub("\\.", " ", colnames(out))[1]
    }
    out
}

getDataCol <- function (ws) {
    dclist <- NULL
    dc <- getData(ws,dcol=1);
    if (nchar(msgError)==0) {
        if (length(dc$Subset)==1 && dc$Subset=="collection") {
           collection <- getData(ws,'/collection',dcol=1);
           collection$url[is.na(collection$url)] <- externalURL
           dclist <- list(collection=dc, list=collection)
        }
    }
    dclist
}

getInit <- function() {
    # Get subsets information
    subsets <<- getData(ws, 'subset')
    if (nchar(msgError)==0) {
       subsets <<- subsets[order(subsets$SetID),]
       subsetNames <<- .C(subsets$Subset)
       connectList <<- cbind( subsets[subsets$LinkID>0, ]$LinkID , subsets[subsets$LinkID>0, ]$SetID )
       subsets$LinkID <<- NULL
       # Filtering of subset depending on quantitative attributes
       setnames <<- as.vector(subsets[,'Subset'])
       nq <- simplify2array(lapply(setnames, function (x) { V <- as.vector(getData(ws,paste('(',x,')/quantitative',sep=''))[,1]); length(V[V==x]); }))
       setnames <<- setnames[ nq>0 ]
       subsets <<- subsets[ subsets[, 'Subset'] %in% setnames, ]
       DSL <<- c(0,subsets$SetID)
       names(DSL) <<- c('-- Select a Data Subset --',.C(subsets$Description))
       subsets$SetID <<- NULL
       subsets[,5] <<- sapply(.C(subsets[,5]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
       subsets[,6] <<- sapply(.C(subsets[,6]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
       dn <<- fillDN(dn, min(connectList[,1]))
       Lev <- NULL; Lev <- cntLevelDN(Lev, dn , 1); N <- min(max(Lev),25)
       N <- (trunc(N/5)+1*(N %% 5 >0))*5
       fs <<- -N + 45
    }
}

getVars <- function(setID, rmvars=FALSE) {

    inDSselect <<- setID
    setName <- subsetNames[setID]

    # Get DATA
    data <<- getData(ws,paste('(',setName,')',sep=''))
    if (dim(data)[2]<=maxVariables) {
       # Get Samples: attribute features, list of identifiers
       I <- getData(ws,paste('(',setName,')/identifier',sep=''))
       samplename <<- I[I$Subset == setName, ]
       samples <<- .C(samplename$Attribute)
       S <<- unique(data[ , samples])
       S <<- S[ order(S) ]
       
       # Get quantitative variable features
       Q <- getData(ws,paste('(',setName,')/quantitative',sep=''))
       varnames <<- Q[Q$Subset == setName, ]
       
       # Get qualitative variable features
       Q <- getData(ws,paste('(',setName,')/qualitative',sep=''))
       #qualnames <<- Q[Q$Subset == setName, ]
       qualnames <<- Q
       
       # Get factor features
       facnames <<- getData(ws,paste('(',setName,')/factor',sep=''))
       
       # Get all qualitative features
       features <<- rbind(I, facnames, qualnames)
       
       # Merge all labels
       LABELS <<- rbind(
         matrix( c( as.matrix(samplename)[,c(2:3)], 'Identifier', as.matrix(samplename)[,c(5:6)]), ncol=5, byrow=FALSE  ),
         matrix( c( as.matrix(facnames)[,c(2:3)], replicate(dim(facnames)[1],'Factor'  ), as.matrix(facnames)[,c(5:6)] ), ncol=5, byrow=FALSE  ),
         matrix( c( as.matrix(varnames)[,c(2:3)], replicate(dim(varnames)[1],'Variable'), as.matrix(varnames)[,c(5:6)] ), ncol=5, byrow=FALSE  )
       )
       if (dim(as.matrix(qualnames))[1]>0 ) LABELS <<- rbind ( LABELS, matrix( c( as.matrix(qualnames)[,c(2:3)], replicate(dim(qualnames)[1],'Feature'), as.matrix(qualnames)[,c(5:6)] ), ncol=5, byrow=FALSE ) )
       colnames(LABELS) <<- c( 'Attribute', 'Description', 'Type', 'CV_Term_ID ', 'CV_Term_Name' )
       LABELS[,4] <<- sapply(.C(LABELS[,4]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
       LABELS[,5] <<- sapply(.C(LABELS[,5]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
       
       # Numerical conversion
       for( i in 1:dim(varnames)[1]) { if (.C(varnames$Type[i]) == 'numeric') data[,.C(varnames$Attribute[i])] <<- .N(data[,.C(varnames$Attribute[i])]); }
       for( i in 1:dim(samplename)[1]) { if (.C(samplename$Type[i]) == 'numeric') data[,.C(samplename$Attribute[i])] <<- .N(data[,.C(samplename$Attribute[i])]); }
       
       # Remove quantitative variables with all values at zero
       if (rmvars){
          V <- simplify2array( lapply(varnames$Attribute, function(v) { sum( which(data[, .C(v)]!=0) ) }) )
          if (length(which(V==0))>0) {
             data <<- data[, ! colnames(data) %in% .C(varnames$Attribute[ c(which(V==0))]) ]
             LABELS <<- LABELS[! LABELS[,1] %in% .C(varnames$Attribute[c(which(V==0))]), ]
             varnames <<- varnames[ -c(which(V==0)), ]
          }
       }
    } else {
       varnames <<- t(data[1:2,])
    }
}

getLabels <- function() {
    labelinfo <- NULL
    for( i in 1:dim(LABELS)[1]) {
        linkOnto <- ifelse( nchar(.C(LABELS[i,4]))>0, paste("<a href='",.C(LABELS[i,4]),"' target='_blank'>[", basename(.C(LABELS[i,4])),'] ', .C(LABELS[i,5]),"</a>", sep=""), "-")
        labelinfo <- rbind( labelinfo , c( .C(LABELS[i,c(1:3)]), linkOnto ) )
    }
    df <- as.data.frame(labelinfo)
    names(df) <- c("Attribute","Description","Type","CV_Term")
    df
}

fillDN <- function( dn, indx) {
    dn$name <- subsetNames[ indx ]
    L <- as.vector(connectList[ connectList[,1]==indx, 2])
    if (length(L)>0) {
       dn$children <- list()
       for (i in 1:length(L)) {
           dn$children[[i]] <- list()
           dn$children[[i]] <- fillDN( dn$children[[i]], L[i])
       }
    }
    dn
}

cntLevelDN <- function(Lev, dn, levelid) {
    if (length(dn$children)>0) {
        if (is.null(Lev[levelid]) || is.na(Lev[levelid])) Lev[levelid] <- 0
        Lev[levelid] <- Lev[levelid] + length(dn$children)
        levelid <- levelid + 1
        for (i in 1:length(dn$children)) {
              Lev <- cntLevelDN(Lev, dn$children[[i]], levelid )
        }
    }
    Lev
}

