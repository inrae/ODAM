
options(width=256)
options(warn=-1)
options(stringsAsFactors=FALSE)

if (! "Rodam" %in% .packages(all.available = TRUE)) {
    require(devtools)
    install_github("inrae/Rodam")
}
library(Rodam)
library(foreach)
library(doParallel)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
.N <- function(x) { as.numeric(as.vector(x)) }
.C <- function(x) { as.vector(x) }
.J <- function(x) { paste(as.vector(x),collapse=',') }
.S <- function(x) { strsplit(x,',')[[1]] }

#-------------------

# Initialize the 'ODAM' object 
dh <- new('odamws','http://10.0.0.104/getdata/', 'FR17KW007', maxtime=30)

# Get 'nmr_metabo' data subset
ds <- dh$getSubsetByName('nmr_metabo')

# Boxplot of all variables defined in ds$varnames
Rank <- simplify2array(lapply(ds$varnames, function(x) { round(mean(log10(ds$data[ , x]), na.rm=T)) }))
Rank[!is.finite(Rank)] <- 0
colRank <- Rank - min(Rank) + 1
cols <- c('red', 'orange', 'darkgreen', 'blue', 'purple', 'brown', 'darkblue', 'darkred', 'darkorange')
boxplot(log10(ds$data[, ds$varnames]), outline=F, horizontal=T, border=cols[colRank], las=2, cex.axis=0.5)
ColVars <- NULL
for(x in 1:length(ds$varsBySubset)) ColVars <- c(ColVars, rep(cols[x], length(ds$varsBySubset[[x]])))
for(x in 1:length(ColVars)) axis(side=2, at=x, col.axis=ColVars[x], labels= ds$varnames[x] , las=2, cex.axis=0.5)

#-------------------

ptype <- 0
tadj <- 'BH'

facname <- 'DayAfterAnthese'
faclevs <- unique( sort(ds$data[ , facname ]) )
faclevref <- 76
faclevcomp <- 95
nblevs <- length(faclevs)

subsets <- names(ds$varsBySubset)
varnames <- ds$varnames
varlabels <- gsub(" \\(.+\\)", "", ds$LABELS[ds$LABELS$Attribute %in% ds$varnames, ]$Description)

data <- unique( ds$data[ , c(facname,varnames) ] )
datafac <- ds$data[ , facname ]
datavar <- ds$data[ , varnames ]

new_mats<-c()
new_mats[1] <- list( datavar[datafac==faclevref,] )
new_mats[2] <- list( datavar[datafac==faclevcomp,] )

#  Calculate the means
means <- t(simplify2array( lapply(1:length(new_mats), function(x) { apply(new_mats[[x]],2,mean,na.rm=TRUE) }) ))
dimnames(means) <- list(faclevs[c(faclevref, faclevcomp)],colnames(datavar))

#  Calculate the fold change
folds <- t(apply(means, 1, function(x) { x/means[1,] } ))
dimnames(folds) <- list(rownames(means),colnames(means))

means
folds

foreach::registerDoSEQ()
pvals <- matrix(nrow=ncol(datavar), ncol=1)
pvals[,1] <- foreach(k=1:nrow(pvals), .combine=c) %dopar% {
     tryCatch({
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
     }, error=function(e) { v <- 0.9999 })
     v
}

dat <- data.frame( .N(t(folds)[, 2]), .N(pvals[,1]), varnames, varlabels, rep("mid", length(varnames)), rep(subsets[1], length(varnames)) )
colnames(dat) <- c( 'FoldChange', 'pvalue', 'Vars', 'Name', 'diffexpr', 'subsets' )

dat
