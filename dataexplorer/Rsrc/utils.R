### Parse the section 'section' within the 'INI.file' file
#   Get the INI.list as an initial list to add or replace the couple of values (key=value)
Parse.INI <- function(INI.file, INI.list=list(), section="GLOBAL")
{
  connection <- file(INI.file)
  Lines  <- readLines(connection)
  close(connection)

  Lines <- chartr("[]", "==", Lines)  # change section headers

  connection <- textConnection(Lines)
  d <- read.table(connection, as.is = TRUE, sep = "=", fill = TRUE)
  close(connection)

  L <- d$V1 == ""                    # location of section breaks
  d <- subset(transform(d, V3 = V2[which(L)[cumsum(L)]])[1:3], V1 != "")
  d <- d[d$V3 == section,]

  #INI.list <- list()
  for( i in 1:nrow(d) ) {
       if (! is.na(suppressWarnings(as.numeric(d$V2[i])))) {
           eval(parse(text=paste0('INI.list$',d$V1[i], '<-', as.numeric(d$V2[i]))))
           next
       }
       if (! is.na(suppressWarnings(as.logical(d$V2[i])))) {
           eval(parse(text=paste0('INI.list$',d$V1[i], '<-', as.logical(d$V2[i]))))
           next
       }
       eval(parse(text=paste0('INI.list$',d$V1[i], '<-"', d$V2[i],'"')))
  }
  return(INI.list)
}
