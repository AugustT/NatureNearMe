## Get all hectads data for the past 5 years
getAllData <- function(dir=getwd(), startyear=2008, verbose=TRUE, hectads = NULL){
  
  library(rnbn)
  # load the names of all hectad
  if(is.null(hectads)) load('hectads.rdata')
  count=1
  total=length(hectads)
  
  for(i in hectads){
    
    if(verbose) cat('Getting', count, 'of', total, '\n')
    occ <- getOccurrences(gridRef=i, startYear=startyear, silent=TRUE, acceptTandC=TRUE, latLong=FALSE)
    filename <- file.path(dir, i)
    save(occ, file=filename)
    count = count+1
    
  }

}