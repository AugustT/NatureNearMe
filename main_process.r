# Version 0.1 01/06/2014
# add #rpsls
# in case of failed dm make sure written grif ref file captures this

rm(list=ls())

# Load libraries and data
library(gdata)
library(twitteR)
library(plyr)
library(devtools)
library(sp)
#install_github('rnbn','JNCC-UK')
library(rnbn)
load('datum_vars.rdata')
load('helmert_trans_vars.rdata')
# Source all functions
for (nm in list.files('Scripts')){
  source(file.path('Scripts', nm))
}

# end keeps the loop going
end <- FALSE
# get the ID of the latest record
sinceID <- attr(getTweets(sinceID=NULL), 'max_sinceID')
# get the since id for direct messages
dms <- dmGet(sinceID = NULL)
sinceID_cmd <- dms[[1]]$id
# manually login to the nbn
# change this once bug fixed
nbnLogin()

while(end == FALSE){
  
  a <- 1
  commandsSinceID <- sinceID
  myFriendships <- getFollowers(user = 'NatureNearMe')
  followers <- myFriendships$screen_name
  
  while(a < 20){
    
    # Get tweets  
    tweets <- getTweets(sinceID=sinceID)
    #save(tweets, file='tweets')
    #load(file='tweets')
    print(tweets$text)
    
    if(!is.null(tweets)){
      
      # Save tweets to file
      if(file.exists('tweets.csv')){
        write.table(x = tweets, file = 'tweets.csv', row.names = FALSE, col.names = FALSE, append = TRUE, sep=',')
      } else {
        write.table(x = tweets, file = 'tweets.csv', row.names = FALSE, col.names = TRUE, append = TRUE, sep=',')
      }
      
      # Create grid reference replies
      repliesGR <- gridRefTweets(tweets=tweets, followers=followers) 
      
      # Create and make map replies
      maptwts <- mapTweets(tweets)
      
      # Create and make naturenearme replies
      nearbytwts <- nearbyNature(tweets, followers=followers)
      
      # Directions to nearest
      dirtwts <- directionTweets(tweets)
      
      # Record sinceID we got up to
      sinceID <- attr(tweets, 'max_sinceID')
    }
    
    # Wait for 5.5 seconds, this keeps us under the rate limit
    Sys.sleep(5.5)
    a=a+1
  }
  
  # Get dm and look for commands
  cat('checking for commands\n')
  sinceID_cmd <- getCommands(sinceID = sinceID_cmd)
  
}

# If we exit the loop send me a message
# dm_resp <- try(dmSend(text='@NatureNearMe has quit', user = 'TomAugust85'), silent = TRUE)
