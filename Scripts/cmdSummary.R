cmdSummary <- function(x=NULL){
  
  now <- Sys.Date()
  
  gridref <- read.csv('gridref.csv')
  gridref <- gridref[as.Date(gridref$created) == now,]
  
  map <- read.csv('maps.csv')
  map <- map[as.Date(map$created) == now,]
  
  naturenearme <- read.csv('natureNearMe.csv')
  naturenearme <- naturenearme[as.Date(naturenearme$created) == now,]
  
  tweets <- read.csv('tweets.csv')
  tweets <- tweets[as.Date(tweets$created) == now,]
  
  summaryText <- paste('Tweets', nrow(tweets), '/ Gridref', nrow(gridref),
                       '/ Map', nrow(map), '/ NatureNearMe', nrow(naturenearme))
  
  dm_resp <- try(dmSend(text=summaryText, user = 'TomAugust85'), silent = TRUE)
  
  if(class(dm_resp) == 'try-error'){
    cat('Trying to send summary again\n')
    Sys.sleep(10)
    dm_resp <- try(dmSend(text=summaryText, user = 'TomAugust85'), silent = TRUE)
  }
}