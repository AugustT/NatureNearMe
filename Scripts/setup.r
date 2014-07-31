setup <- function(){

  # Load libraries and data
  library(gdata)
  library(twitteR)
  library(plyr)
  library(devtools)
  library(sp)
  library(rnbn)
  load('datum_vars.rdata') # datum_vars
  load('helmert_trans_vars.rdata') # helmert_trans_vars
  # Source all functions
  for (nm in list.files('Scripts')){
    source(file.path('Scripts', nm))
  }

  # get the ID of the latest record
  sinceID <- attr(getTweets(sinceID=NULL), 'max_sinceID')
  # get the since id for direct messages
  dms <- dmGet(sinceID = NULL)
  sinceID_cmd <- dms[[1]]$id
  
  return(list(sinceID=sinceID, 
              sinceID_cmd=sinceID_cmd, 
              datum_vars=datum_vars, 
              helmert_trans_vars=helmert_trans_vars))
}