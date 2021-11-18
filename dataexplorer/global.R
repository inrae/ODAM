library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(networkD3)
library(plotly)
library(markdown)

options(width=128)
options(shiny.deprecation.messages=FALSE)
options(shiny.sanitize.errors = FALSE)
#options(error=function() { traceback(2); quit("no", status = 1, runLast = FALSE) })

source("Rsrc/utils.R")
conffile <- "conf/global.ini"
conf <- Parse.INI(conffile, section="GLOBAL")

idVersion <- conf$VERSION

# API URL
externalURL <- conf$GETDATA_URL_PROXY

# determines whether curl verifies the authenticity of the peer's certificate
SSL_VerifyPeer <- conf$SSL_VERIFYPEER

# Maximum number of variables in a data subset so that it  can be explored interactively
maxVariables <- conf$MAXVARIABLES

# Nb max item when multiselect
nbopt_multiselect <- 250

# Value of the pseudo zero to apply a log10
pseudo_zero <- 0.0001

# Save plots (GGM & COR)
saveplots <- FALSE