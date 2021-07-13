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
library(magrittr)
library(htmlwidgets)
library(htmltools)
library(ggpubr)
library(ggdendro)
library(stringr)
library(FastGGM)
library(RcppParallel)

setThreadOptions(numThreads = 4) # set 4 threads for parallel computing

# data set / data collection
dsname <- ''
dcname <- ''

# ws : Web service connection variables
# 1: API key mode
# 2: dataset shortname
# 3: API Key
# 4: API URL
# 5: data collection shortname
# 6: the client's originating IP
# 7,8,9 : selection of the subset, menu item, header type
ws <- c(0, dsname, '', externalURL, dcname, '', NULL, NULL, NULL)

# global variables related to the dataset 
dclist <- NULL
inDselect <- NULL
inDSselect <- 0
data <- NULL
subsets <- NULL
subsetNames <- NULL
samplename <- NULL
samples <- NULL
S <- NULL
varnames <- NULL
setnames <- NULL
facnames <- NULL
features <- NULL
LABELS <- NULL
DSL <- NULL
connectList <- NULL
dn <- NULL

# Error message return by the API
msgError <- ''

# Font size for the Subsets Graph
fs <- 10

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
.N <- function(x) { as.numeric(as.vector(x)) }
.C <- function(x) { as.vector(x) }

# Was there an error in the web services?
is.wsError <- function() return( ifelse(nchar(msgError)>0, TRUE, FALSE ) )

is.wsNoAuth <- function() return( ifelse(length(grep("invalid authorization key", msgError)), TRUE, FALSE ) )

# Is a dataset / data collection specified in the query string ?
is.DS <- function(cdata) {
    lparams <- unlist(strsplit(gsub("\\?", "", cdata[['url_search']]),  '&'))
    ret <- FALSE
    if (!is.na(pmatch('ds', lparams)) || !is.na(pmatch('dc', lparams))) ret <- TRUE
    return(ret)
}

# Get query string parameters
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
    auth <- ''; ApiKeyMode <- 0
    if (!is.null(params[['auth']])) {
        auth <- params[['auth']]
        ApiKeyMode <- 1
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
    c(  ApiKeyMode, dsname, auth, externalURL, dcname, '', subsetname, tabname, headerflag )
}

# Low level routine allowing to retrieve data or metadata from  a query formatted according the ODAM framework specifications
httr_get <- function(ws, query, mode='text', fsplit=TRUE) {
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
getAbout <- function () {
    aboutfile <- '/srv/shiny-server/www/about.md'
    if(file.exists(aboutfile)){
       gsub('@@IMAGE@@/', '', readLines(aboutfile, n = -1) )
    }
}

# Get 'infos.md' content
getInfos <- function (ws) {
    T <- httr_get(ws, paste0('infos/', ws[2]))
    if (!is.wsError() && !is.wsNoAuth()) {
       # Images
       P <- na.omit(str_extract(T, pattern="@@IMAGE@@/[^\\.]+\\.(png|jpg)"))
       if (length(P)>0) for (i in 1:length(P)) {
             I <- base64_enc(httr_get(ws, paste0('image/', ws[2], '/', gsub('@@IMAGE@@/','',P[i])), mode='raw'))
             T <- gsub( P[i], paste0('data:image/png;base64,',I), T )
       }
       # PDF - markdown link style
       P <- na.omit(str_extract(T, pattern="\\[[^\\]]+\\]\\(@@PDF@@/[^\\.]+\\.pdf\\)"))
       if (length(P)>0) for (i in 1:length(P)) {
          V <- as.vector(simplify2array(strsplit(gsub('@@PDF@@','',gsub('(\\[|\\]|\\(|\\))','',P[i])),'/')))
          urlapi <- paste0(ws[4],'pdf/',ws[2],'/',V[2])
          href <- paste0("<a class=\"jlink\" onclick=\"javascript:openPDF('",urlapi,"');\">",V[1],"</a>")
          T <- gsub(P[i], href, T, fixed=TRUE)
       }
       # PDF - normal link style
       P <- na.omit(str_extract(T, pattern="@@PDF@@/[^\\.]+\\.pdf"))
       if (length(P)>0) for (i in 1:length(P)) {
          V <- gsub('@@PDF@@/','',P[1])
          urlapi <- paste0(ws[4],'pdf/',ws[2],'/', V)
          href <- paste0("<a class=\"jlink\" onclick=\"javascript:openPDF('",urlapi,"');\">",V,"</a>")
          T <- gsub(P[i], href, T, fixed=TRUE)
       }
    }
    T
}

# Get tabulated data 
getData <- function (ws, query='', dcol=0) {
    out <- data.frame()
    repeat {
       msgError <<- ''
       ds <- ifelse(dcol>0, ws[5], ws[2])
       T <- httr_get(ws, paste0('tsv/',ds,'/',query))
       if (length(grep("ERROR", T[1]))) {
          msgError <<- T[1]
          break
       }
       out <- read.csv(textConnection(T), head=TRUE, sep="\t")
       if (dim(out)[1]==0) {
          msgError <<- gsub("\\.", " ", colnames(out))[1]
       }
       break
    }
    out
}

# Get tabulated data about data collection
getDataCol <- function (ws) {
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
getInit <- function() {
    # Get subsets information
    subsets <<- getData(ws, 'subset')
    if (!is.wsError()) {
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

# Get tabulated data and metadata regarding a data subset
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

# Convert/Format LABELS (metadata) as a data.frame along their url links
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

# Build a tree of relations between data subsets
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

