library(jsonlite)
library(RCurl)
library(httr)
library(reshape2)
library(pcaMethods)
library(grid)
library(ellipse)
library(ggplot2)
library(svglite)
library(JADE)
library(moments)
library(scales)
library(igraph)
library(magrittr)
library(htmlwidgets)
library(htmltools)
library(ggpubr)
library(ggdendro)
library(stringr)
library(FastGGM)
library(RcppParallel)

setThreadOptions(numThreads = 4) # set 4 threads for parallel computing

# ws : Web service connection variables
# 1: API key mode : 0 => no API key, 1 => API key in query string, 2 => API key in HTTP header
# 2: dataset shortname
# 3: API Key
# 4: API URL
# 5: data collection shortname
# 6: the client's originating IP
# 7,8,9 : selection of the subset, menu item, header type
# 10-15: ui, analysis type, factors, variables
ws <- c(0, '', '', externalURL, '', '', NULL, NULL, NULL,'','','','','','')

# global variables
g <- list(
   # variables related to the dataset
   dclist=NULL,
   inDselect=NULL,
   inDSselect='',
   data=NULL,
   subsets=NULL,
   subsets2=NULL,
   subsetNames=NULL,
   samplename=NULL,
   samples=NULL,
   S=NULL,
   identifiers=NULL,
   varnames=NULL,
   varsBySubset=NULL,
   setnames=NULL,
   facnames=NULL,
   features=NULL,
   LABELS=NULL,
   DSL=NULL,
   connectList=NULL,
   dn=NULL,

   # Error message return by the API
   msgError='',

   # Font size for the Subsets Graph
   fs=10
)

outfiles <- list('PCA'='multi.html', 'ICA'='multi.html','COR'='corr.svg','GGM'='ggm.html')

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
.N <- function(x) { as.numeric(as.vector(x)) }
.C <- function(x) { as.vector(x) }
.J <- function(x) { paste(as.vector(x),collapse=',') }
.S <- function(x) { strsplit(x,',')[[1]] }

# Was there an error in the web services?
is.wsError <- function() return( ifelse(nchar(g$msgError)>0, TRUE, FALSE ) )

is.wsNoAuth <- function() return( ifelse(length(grep("invalid authorization key", g$msgError)), TRUE, FALSE ) )

# Is a dataset / data collection specified in the query string ?
is.DS <- function(cdata)
{
    lparams <- unlist(strsplit(gsub("\\?", "", cdata[['url_search']]),  '&'))
    ret <- FALSE
    if (!is.na(pmatch('ds', lparams)) || !is.na(pmatch('dc', lparams))) ret <- TRUE
    return(ret)
}

# Get query string parameters
getWS <- function(cdata)
{
    params <- parseQueryString(cdata$url_search)
    if (!is.null(params[['ws']]))
        externalURL <<- params[['ws']]

    dcname <- ''
    if (!is.null(params[['dc']]))
        dcname <- params[['dc']]

    dsname <- ''
    if (!is.null(params[['ds']]))
        dsname <- params[['ds']]

    auth <- ''; ApiKeyMode <- 0
    if (!is.null(params[['auth']])) {
        auth <- params[['auth']]
        ApiKeyMode <- 1
    }

    subsetname <- ''
    if (!is.null(params[['subset']]))
        subsetname <- params[['subset']]

    tabname <- ''
    if (!is.null(params[['tab']]))
        tabname <- params[['tab']]

    headerflag <- ''
    if (!is.null(params[['frame']]))
        headerflag <- params[['frame']]

    uiflag <- ''
    if (!is.null(params[['ui']]))
        uiflag <- params[['ui']]

    analysis <- ''
    if (!is.null(params[['type']])) analysis <- params[['type']]

    factor1 <- ''
    if (!is.null(params[['fac']]))  factor1 <- params[['fac']]
    if (!is.null(params[['fac1']])) factor1 <- params[['fac1']]

    factor2 <- ''
    if (!is.null(params[['fac2']])) factor2 <- params[['fac2']]

    variable1 <- ''
    if (!is.null(params[['var']]))  variable1 <- params[['var']]
    if (!is.null(params[['var1']])) variable1 <- params[['var1']]

    variable2 <- ''
    if (!is.null(params[['var2']])) variable2 <- params[['var2']]

    c(  ApiKeyMode, dsname, auth, externalURL, dcname, '', subsetname, tabname, headerflag, uiflag, 
        analysis, factor1, factor2, variable1, variable2 )
}

# Low level routine allowing to retrieve data or metadata from  a query formatted according the API specifications
httr_get <- function(ws, query, mode='text', fsplit=TRUE)
{
    headers <- c( 'X-Forwarded-For' = ws[6] )
    if (nchar(ws[3])>0) headers <- c( headers, 'X-Api-Key' = ws[3] )
    T <- ''
    tryCatch({
       resp <- httr::GET(paste0(ws[4], query), timeout(5),
                         config = httr::config(ssl_verifypeer = SSL_VerifyPeer),
                         add_headers(.headers = headers))
       T <- httr::content(resp, as=mode)
       if (mode=='text' && fsplit) T <- simplify2array(strsplit(T,"\n"))
    }, error=function(e) {
       T <- "## ERROR : the API host is not responding; it is either not found or does not exist"
    })
    T
}

# Get 'about.md' content
getAbout <- function ()
{
    aboutfile <- '/srv/shiny-server/www/about.md'
    if(file.exists(aboutfile)){
       gsub('@@IMAGE@@/', '', readLines(aboutfile, n = -1) )
    }
}

# Get 'infos.md' content
getInfos <- function (ws)
{
    T <- httr_get(ws, paste0('infos/', ws[2]))
    if (!is.wsError() && !is.wsNoAuth() && ws[1]>0) {
       # Images
       P <- na.omit(str_extract(T, pattern="https?:[^:]+\\.(png|jpg)"))
       if (length(P)>0) for (i in 1:length(P)) {
             I <- base64_enc(httr_get(ws, paste0('image/', ws[2], '/', basename(P[i])), mode='raw'))
             T <- gsub( P[i], paste0('data:image/png;base64,',I), T )
       }
       # PDF - markdown link style
       P <- na.omit(str_extract(T, pattern="\\[[^\\]]+\\]\\(https?:[^:]+\\.pdf\\)"))
       if (length(P)>0) for (i in 1:length(P)) {
          V <- as.vector(simplify2array(strsplit(gsub('(\\[|\\]|\\(|\\))',',',P[i]),',')))
          V <- V[ V != "" ]
          href <- paste0("<a class=\"jlink\" onclick=\"javascript:openPDF('",V[2],"');\">",V[1],"</a>")
          T <- gsub(P[i], href, T, fixed=TRUE)
       }
       # PDF - normal link style
       P <- na.omit(str_extract(T, pattern="[^']https?:[^:]+\\.pdf"))
       if (length(P)>0) for (i in 1:length(P)) {
          urlapi <- substring(P[i],2)
          href <- paste0(substring(P[i],1,1), "<a class=\"jlink\" onclick=\"javascript:openPDF('",urlapi,"');\">",basename(urlapi),"</a>")
          T <- gsub(P[i], href, T, fixed=TRUE)
       }
    }
    T
}

# Get tabulated data
getData <- function (ws, query='', dcol=0)
{
    out <- data.frame()
    repeat {
       g$msgError <<- ''
       ds <- ifelse(dcol>0, ws[5], ws[2])
       T <- httr_get(ws, paste0('tsv/',ds,'/',query))
       if (length(grep("ERROR", T[1]))) {
          g$msgError <<- T[1]
          break
       }
       out <- read.csv(textConnection(T), head=TRUE, sep="\t")
       if (nrow(out)==0) {
          g$msgError <<- gsub("\\.", " ", colnames(out))[1]
       }
       break
    }
    out
}

# Get tabulated data about data collection
getDataCol <- function (ws)
{
    dclist <- NULL
    dc <- getData(ws,dcol=1);
    if (!is.wsError()) {
        if (length(dc$Subset)==1 && dc$Subset=="collection") {
           collection <- getData(ws,'/collection',dcol=1);
           collection$url[is.na(collection$url)] <- externalURL
           dclist <- list(collection=dc, list=collection)
        }
    }
    dclist
}

# Get tabulated data about data subsets
getInit <- function()
{
    # Get subsets information
    g$subsets <<- getData(ws, 'subset')
    if (!is.wsError()) {
       g$subsets <<- g$subsets[order(g$subsets$SetID),]
       g$subsetNames <<- .C(g$subsets$Subset)
       g$connectList <<- cbind( g$subsets[g$subsets$LinkID>0, ]$LinkID , g$subsets[g$subsets$LinkID>0, ]$SetID )
       # Filtering of subset depending on quantitative attributes
       g$setnames <<- as.vector(g$subsets[,'Subset'])
       nq <- simplify2array(lapply(g$setnames, function (x) { V <- as.vector(getData(ws,paste('(',x,')/quantitative',sep=''))[,1]); length(V[V==x]); }))
       g$setnames <<- g$setnames[ nq>0 ]
       g$subsets2 <<- g$subsets[ g$subsets[, 'Subset'] %in% g$setnames, ]
       g$subsets2 <<- g$subsets2[ , ! colnames(g$subsets2) %in% c('SetID','LinkID') ]
       g$DSL <<- .C(g$subsets2$Subset)
       names(g$DSL) <<- .C(g$subsets2$Description)
       g$subsets[,5] <<- sapply(.C(g$subsets[,5]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
       g$subsets[,6] <<- sapply(.C(g$subsets[,6]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
       g$dn <<- fillDN(g$dn, min(g$connectList[,1]))
       Lev <- NULL; Lev <- cntLevelDN(Lev, g$dn , 1); N <- min(max(Lev),25)
       N <- (trunc(N/5)+1*(N %% 5 >0))*5
       g$fs <<- -N + 45
    }
}

# Get tabulated data and metadata regarding a subset name list
getVars <- function(strNameList, rmvars=FALSE)
{
    if (g$inDSselect != strNameList)
    {
       # Get Name List
       g$inDSselect <<- strNameList
       setNameList <- .S(strNameList)

       # Get DATA
       data <- getData(ws,paste('(',strNameList,')',sep=''))
       if (ncol(data)<=maxVariables)
       {
          # Get quantitative variable features
          varnames <- NULL
          Q <- getData(ws,paste('(',strNameList,')/quantitative',sep=''))
          for( i in 1:length(setNameList) ) varnames <- rbind(varnames,  Q[Q$Subset == setNameList[i], ])

          # Get qualitative variable features
          qualnames <- getData(ws,paste('(',strNameList,')/qualitative',sep=''))
          g$qualnames <<- qualnames

          # Get factor features
          facnames <- getData(ws,paste('(',strNameList,')/factor',sep=''))
          g$facnames  <<- facnames

          # Get all identifiers
          identifiers <- getData(ws,paste('(',strNameList,')/identifier',sep=''))
          g$identifiers <<- identifiers

          # Gather all qualitative features
          features <- rbind(identifiers, facnames, qualnames)
          g$features  <<- features

          # Get Samples: attribute features, list of identifiers
          L <- NULL
          if (length(setNameList)==1) {
              L[1] <- g$subsets[ g$subsets$Subset==setNameList[1], ]$SetID
          } else for( i in 1:length(setNameList) ) {
              l <- c( g$subsets[ g$subsets$Subset==setNameList[i], ]$SetID )
              while( l[length(l)]>0 ) l <- c(l, g$subsets[ g$subsets$SetID==l[length(l)], ]$LinkID )
              if (i==1) {
                 L <- l
              } else {
                  L <- l[l %in% L]
              }
          }
          g$samples <<- .C(g$subsets[ g$subsets$SetID==L[1], ]$Identifier)
          g$S <<- unique(data[ , g$samples])
          g$S <<- g$S[ order(g$S) ]

          setName <- g$subsets[ g$subsets$SetID==L[1], ]$Subset
          Q <- getData(ws,paste('(',setName,')/identifier',sep=''))
          samplename <- Q[Q$Subset %in% setName, ]
          g$samplename <<- samplename

          # Merge all labels
          LABELS <- rbind(
             matrix( c( as.matrix(identifiers)[,c(1:4)], replicate(nrow(identifiers),'Identifier' ), as.matrix(identifiers)[,c(6:7)]), ncol=7, byrow=FALSE  ),
             #matrix( c( as.matrix(samplename)[,c(1:4)], 'Identifier', as.matrix(samplename)[,c(6:7)]), ncol=7, byrow=FALSE  ),
             matrix( c( as.matrix(facnames)[,c(1:4)], replicate(nrow(facnames),'Factor'  ), as.matrix(facnames)[,c(6:7)] ), ncol=7, byrow=FALSE  ),
             matrix( c( as.matrix(varnames)[,c(1:4)], replicate(nrow(varnames),'Variable'), as.matrix(varnames)[,c(6:7)] ), ncol=7, byrow=FALSE  )
          )
          if (nrow(as.matrix(qualnames))>0 ) { LABELS <- rbind ( LABELS,
             matrix( c( as.matrix(qualnames)[,c(1:4)], replicate(nrow(qualnames),'Feature'), as.matrix(qualnames)[,c(6:7)] ), ncol=7, byrow=FALSE )
          )}
          colnames(LABELS) <- c( 'Subset', 'Attribute', 'WSEntry', 'Description', 'Type', 'CV_Term_ID ', 'CV_Term_Name' )
          LABELS[,6] <- sapply(.C(LABELS[,6]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
          LABELS[,7] <- sapply(.C(LABELS[,7]), function(x) { ifelse( ! is.na(x), x, "NA" ); })
          LABELS <- as.data.frame(LABELS)

          varsBySubset <- list()
          for(setName in setNameList)
               varsBySubset[[setName]] <- .C(varnames$Attribute[ varnames$Attribute %in% LABELS[ LABELS$Subset==setName, ]$Attribute ])
          g$varsBySubset <<- varsBySubset

          for( i in 1:nrow(varnames)) { if (.C(varnames$Type[i]) == 'numeric') data[,.C(varnames$Attribute[i])] <- .N(data[,.C(varnames$Attribute[i])]); }
          for( i in 1:nrow(samplename)) { if (.C(samplename$Type[i]) == 'numeric') data[,.C(samplename$Attribute[i])] <- .N(data[,.C(samplename$Attribute[i])]); }

          # Remove quantitative variables with all values at zero
          if (rmvars) {
             V <- simplify2array( lapply(varnames$Attribute, function(v) { sum( which(data[, .C(v)]!=0) ) }) )
             if (length(which(V==0))>0) {
                data <- data[, ! colnames(data) %in% .C(varnames$Attribute[ c(which(V==0))]) ]
                LABELS <- LABELS[! LABELS[,1] %in% .C(varnames$Attribute[c(which(V==0))]), ]
                varnames <- varnames[ -c(which(V==0)), ]
             }
          }
          g$data <<- data
          g$LABELS <<- LABELS
          g$varnames  <<- varnames
       } else {
          g$varnames <<- t(data[1:2,])
       }
    }
}

# Convert/Format LABELS (metadata) as a data.frame along their url links
getLabels <- function() {
    labelinfo <- NULL
    LABELS <- g$LABELS
    for( i in 1:nrow(LABELS)) {
        linkOnto <- ifelse( nchar(.C(LABELS[i,6]))>0, 
                            paste("<a href='",.C(LABELS[i,6]),"' target='_blank'>[", basename(.C(LABELS[i,6])),'] ', .C(LABELS[i,7]),"</a>", sep=""), 
                            "-")
        labelinfo <- rbind( labelinfo , c( .C(LABELS[i,c(2,4,3,5)]), linkOnto ) )
    }
    df <- as.data.frame(labelinfo)
    names(df) <- c("Attribute","Description","WSEntry","Category","CV_Term")
    df
}

# Build a tree of relations between data subsets
fillDN <- function( dn, indx) {
    dn$name <- g$subsetNames[ indx ]
    L <- as.vector(g$connectList[ g$connectList[,1]==indx, 2])
    if (length(L)>0) {
       dn$children <- list()
       for (i in 1:length(L)) {
           dn$children[[i]] <- list()
           dn$children[[i]] <- fillDN( dn$children[[i]], L[i])
       }
    }
    dn
}

# Compute the tree deepth
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

