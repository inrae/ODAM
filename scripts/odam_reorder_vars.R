#-------------------------------------------------
# Name        : odam_reorder_vars
#
# Description : * Given that the number of variables is limited to 1000 in the data explorer and this
#                 to remain fluid in the interaction with the data, only the first 1000 variables 
#                 belonging to the chosen data subset will be selected from the attribute file.
#               * So, we have to select the most relevant variables belonging to a data subset in order
#                 to put them first in the attribute file (i.e a_attributes.tsv).
#               * The selection of the most relevant variables can be done by two methods: by ANOVA or 
#                 by a Kruskal-Wallis test.
#               * If the number of variables is very important, we can above a threshold switch to 
#                 parallel mode to perform the calculations.
#
# Call : odam_reorder_vars(dataset, datasubset, apikey='', apiurl=APIURL, dataroot=DATAROOT, opt=NULL)
#
#  parameters :
#    * dataset : the dataset name
#    * datasubset : the data subset name
#    * apikey : the APIKEY if needed
#    * apiurl : the URL of the ODAM API for querying data - default value = APIURL
#    * dataroot : Root of the ODAM data directory - default value = DATAROOT
#    * opt : list of some parameters
#        * parsize : Number of variables beyond which the calculation will be parallelized
#        * selvars : Number of relevant variables to be put at first in the a_attributes.tsv file
#        * ncpu    : Number of cpus to allocate if parallelization is required
#        * ptype   : Method used for the selection of variables : 0 for ANOVA and 1 for Kruskal-Wallis Test
#        * tcorr   : Method for p-value adjustment - possible values : none, fdr, bonferroni
#-------------------------------------------------

# EXAMPLES

# source("C:/Workdir/Projects/dataexplorer/Rscripts/odam_reorder_vars.R")

# odam_reorder_vars('metabofla1', 'NMR_buckets')
# odam_reorder_vars('metabofla1', 'NMR_buckets', opt=list(selvars=150))

# odam_reorder_vars('Atacama', 'lcms_esi_neg')
# odam_reorder_vars('Atacama', 'lcms_esi_pos')

# -- Example with messages ---
## > system.time( odam_reorder_vars('metabofla1', 'NMR_buckets', opt=list(selvars=150)) )
## Initialize the 'ODAM' object ...OK
## Get data subset NMR_buckets ...OK 284 variables
## Compute ANOVA for each factor: Cultivar Leaf_position_status Plant_Response Infected_insect FactorialGroup OK
## Select the more significant variables ...OK
## Read the a_attributes.tsv file corresponding to the dataset ...OK
## Reorder variables belondings to the data subset ...OK
## Save the new a_attributes.tsv file ...OK
## utilisateur     système      écoulé 
##        1.80        0.11        2.48 
##


#-------------------------------------------------

# --- Default parameters values : to be adapted ---

# Root of the ODAM data directory
DATAROOT <- "C:/DATA/ODAM/data"

# URL of the ODAM API
APIURL <- 'https://pmb-bordeaux.fr/getdata/'
#APIURL <- 'http://10.0.0.104/getdata/'

# Method used for the selection of variables : 0=ANOVA, 1=Kruskal-Wallis Test
PTYPE <- 0
# Method for p-value adjustment - possible values : 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr', 'none'
TCORR <- 'fdr'

# Number of relevant variables to be put at first in the a_attributes.tsv file
SELVARS <- 1000
# Number of variables beyond which the calculation will be parallelized
PARSIZE <- 500
# Number of cpus to allocate if parallelization is required
NCPU <- 4

#-------------------------------------------------

suppressMessages({
   library(foreach)
   library(doParallel)
   library(Rodam)
})

options(width=256)
options(warn=-1)
options(stringsAsFactors=FALSE)

MYANOVA <- function(ds,facname, opt=NULL)
{
    params <- list(parsize=PARSIZE, selvars=SELVARS, ncpu=NCPU, tcorr=TCORR, ptype=PTYPE)
    if (!is.null(opt) && "list" %in% class(opt))
       for (p in ls(opt)) params[[p]] <- opt[[p]]

    data <- ds$data[ , ds$varnames ]
    factvec <- ds$data[ , facname ]
    nbvars <- length(ds$varnames)

    if (params$ptype==0) {
       aov.fac <- function(x, vfac) {unlist(summary(aov(x ~  vfac)), use.names=F)[c(3,4,9)]; }
    } else {
       aov.fac <- function(x, vfac) { as.numeric(unlist(kruskal.test(x ~ as.factor(vfac)), use.names=F)[c(1,3)]); }
    }
    if (nbvars>params$parsize) {
        aov.vec <- foreach(i=1:nbvars, .combine=rbind) %dopar% { aov.fac(data[,i], factvec)  }
    } else {
        aov.vec <- t(apply(as.matrix(data), 2, function(x) { aov.fac(x, factvec) }));
    }
    if (params$ptype==0) {
       aov.vec <- aov.vec[,3]
    } else {
       aov.vec <- aov.vec[,2]
    }
    if(params$tcorr != "none")
        aov.vec <- p.adjust(aov.vec, params$tcorr)

    ord.inx <- order(as.numeric(aov.vec), decreasing = FALSE);
    aov.vec <- as.matrix(aov.vec[ord.inx]);
    cbind( colnames(data[,ord.inx]), aov.vec )
}

odam_reorder_vars <- function(dataset, datasubset, apikey='', apiurl=APIURL, dataroot=DATAROOT, opt=NULL)
{
   ret <- 0
   tryCatch({
      params <- list(parsize=PARSIZE, selvars=SELVARS, ncpu=NCPU, tcorr=TCORR, ptype=PTYPE)
      if (!is.null(opt) && "list" %in% class(opt))
         for (p in ls(opt)) params[[p]] <- opt[[p]]

    # Initialize the 'ODAM' object 
      cat("Initialize the 'ODAM' object ...")
      if (nchar(apikey)>0) {
         dh <- new('odamws',apiurl, dataset, apikey)
      } else {
         dh <- new('odamws',apiurl, dataset)
      }
      cat("OK\n")

    # Get data subset
      cat(paste0("Get data subset ",datasubset," ..."))
      ds <- dh$getSubsetByName(datasubset)
      cat(paste0("OK ",length(ds$varnames)," variables\n"))

      if ( length(ds$varnames)>params$selvars ) {

       # In case number of variables greater than PARSIZE, allocatate cpus
         if ( length(ds$varnames)>params$parsize ) {
            cat(paste0("Allocate ",params$ncpu,"cpus for parallel computing  ..."))
            registerDoParallel(cores=params$ncpu)
            cat("OK\n")
         }

       # Apply an ANOVA for each variable and for each factor
         cat("Compute ANOVA for each factor: ")
         aov.vec <- NULL
         for (i in 1:length(ds$facname)) {
            cat(ds$facname[i], '')
            aov.vec <- rbind( aov.vec, MYANOVA(ds, ds$facname[i], opt=params)[1:params$selvars,] )
         }
         cat("OK\n")

       # Select the more significant variables
         cat("Select the more significant variables ...")
         nbsel <- min(round(1.5*params$selvars), length(ds$varnames))
         selvars <- unique(aov.vec[order(aov.vec[,2]), ][1:nbsel, 1])
         selvars <- selvars[ 1:min(length(selvars),params$selvars) ]
         cat("OK\n")

       # Merge all variables with the more significant at the top
         ordvars <- c(selvars, ds$varnames[! ds$varnames %in% selvars])

       # Read the a_attributes.tsv file corresponding to the dataset
         cat("Read the a_attributes.tsv file corresponding to the dataset ...")
         attribute_file <- file.path(dataroot,dataset,"a_attributes.tsv")
         attributs <- read.table(attribute_file,header=T, sep="\t")
         cat("OK\n")

       # Reorder variables belondings to the data subset based on ANOVA selection
         cat("Reorder variables belondings to the data subset ...")
         subattrib <- attributs[attributs[,1]==datasubset, ]
         sub1 <- subattrib[ ! subattrib[,4]=='quantitative', ]
         sub2 <- subattrib[  subattrib[,4]=='quantitative', ]
         V <- simplify2array(lapply(ordvars, function(x) { which(sub2[,2]==x) }))
         subattrib2 <- rbind( sub1, sub2[V,] )
         cat("OK\n")

       # Save the new a_attributes.tsv file corresponding to the dataset
         cat("Save the new a_attributes.tsv file ...")
         attributs[attributs[,1]==datasubset, ] <- subattrib2
         write.table(attributs, attribute_file, col.names=T, row.names=F, sep="\t")
         cat("OK\n")
         ret <- 1
      } else {
         cat(paste0("The number variables of the data subset ",datasubset," is less than ",params$selvars,"\n"))
      }
      ret
   }, error=function(e) { paste("ERROR: ", paste(e, collapse=", "),"\n"); })
}
