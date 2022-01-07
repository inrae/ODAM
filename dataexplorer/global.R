library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(networkD3)
library(plotly)
library(markdown)
library(dplyr)
library(shinycssloaders)

options(width=128)
options(shiny.deprecation.messages=FALSE)
options(shiny.sanitize.errors = FALSE)
#options(error=function() { traceback(2); quit("no", status = 1, runLast = FALSE) })

source("Rsrc/utils.R")
conffile <- "conf/global.ini"
conf <- Parse.INI(conffile, section="GLOBAL")

# global variables
globvars <- list(

  debug_shinyjs = FALSE,

  odamdoc_url = 'https://inrae.github.io/ODAM/',

  idVersion = conf$VERSION,

  # API URL
  externalURL = conf$GETDATA_URL_PROXY,

  # determines whether curl verifies the authenticity of the peer's certificate
  SSL_VerifyPeer = conf$SSL_VERIFYPEER,

  # Theme colors
  theme = conf$THEME,

  # Number of Cores
  nbcores = conf$NBCORES,

  # Maximum number of variables in a data subset so that it can be explored interactively
  maxVariables = conf$MAXVARIABLES,

  # Retain the first 'maxVariables' in case of greater
  subsetVars = FALSE,

  # Nb max item when multiselect
  nbopt_multiselect = 150,

  # Nb max variables for COR && GGM analyses
  max_multivars = 150,

  # Save plots (GGM & COR) 
  saveplots = ifelse(conf$SAVEPLOTS==1, TRUE, FALSE),

  # Value of the pseudo zero to apply a log10
  pseudo_zero = 0.0001

)

