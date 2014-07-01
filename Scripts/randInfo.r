### Take in an occurrence dataset and produce random information
## If a TVK is provided it subsets the data to this species and 
## provides species information

randInfo <- function(occ = NULL, TVK = NULL, sp_name = NULL, location = NULL){
  
  if(!is.null(TVK)){
    # subset occ to the species requested
    occ <- occ[occ$pTaxonVersionKey == TVK,]
  }
    
  if(is.null(TVK)){ # Specify the responses appropriate to single or multi taxa
    chance <- sample(1:6,1)
  } else {
    chance <- sample(5:8,1)
  }
  
  if(is.null(location)) location <- 'you'
  
  if(nrow(occ) == 0){
    if(is.null(TVK)){
      text <- paste('There is no data available around', location, ', sorry!')
    } else {
      text <- paste('There are no recent records of', sp_name, 'near', location)
    }
  } else {
    
    if(chance == 1){
      
      ## Get most commonly recorded species
      ## can i get common names?
      commonest_sp_latin <- names(sort(table(occ$pTaxonName),decreasing=TRUE))[1]
      commonest_sp <- getTVKQuery(commonest_sp_latin, top = TRUE)$commonName
      if(is.null(commonest_sp)) commonest_sp <- commonest_sp_latin
      commonest_count <- table(occ$pTaxonName)[commonest_sp_latin][[1]]
      text <- paste('The most common species near', location, 'is', commonest_sp,
                    'it has been recorded', commonest_count, 'times in the last 5 years')
      
    } else if(chance == 2){
      
      ## Get most recent record
      recent <- occ[with(occ, order(as.Date(occ$endDate))),][nrow(occ),]
      name <- getTVKQuery(recent$pTaxonName, top = TRUE)$commonName
      if(is.null(name)) name <- recent$pTaxonName
      text <- paste('The most recent record near', location, 'is of a', name, 'on', recent$endDate)
      
    } else if(chance == 3){
      
      ## Number of species
      number_sp <- length(unique(occ$pTaxonVersionKey))
      text <- paste('There have been', number_sp, 'species recorded near', location, 'in the past 5 years')
      
    } else if(chance %in% 4:6){
      
      ## Random record
      ## Instead of doing it this modify so that is chooses a
      ## random species first then a random record
      random_row <- occ[sample(nrow(occ),1),]
      if(is.na(random_row$pTaxonName)) random_row <- occ[1,]
      name <- getTVKQuery(random_row$pTaxonName, top = TRUE)$commonName
      if(is.null(name)) name <- random_row$pTaxonName
      text <- paste('A', name, 'was recorded near', location, 'on', random_row$endDate)
        
    } else if(chance == 7){
      
      ## Get most recent record for species
      recent <- occ[with(occ, order(as.Date(occ$endDate))),][nrow(occ),]
      text <- paste('The most recent record of', sp_name, 'near', location, 'was on', recent$endDate)
      
    } else if(chance == 8){
      
      # number of records
      text <- paste('There are', nrow(occ), 'records of', sp_name, 'near', location, 'from the past 5 years')
      
    }
  }
  
  return(text)

}