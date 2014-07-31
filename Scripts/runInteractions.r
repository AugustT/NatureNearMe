runInteractions <- function(sinceID = NULL){
  
  # Get tweets  
  tweets <- getTweets(sinceID=sinceID)
  print(tweets$text)
  
  if(!is.null(tweets)){
    
    # Save tweets to file
    if(file.exists('tweets.csv')){
      write.table(x = tweets, file = 'tweets.csv', row.names = FALSE, col.names = FALSE, append = TRUE, sep=',')
    } else {
      write.table(x = tweets, file = 'tweets.csv', row.names = FALSE, col.names = TRUE, append = TRUE, sep=',')
    }
    
    # manually login to the nbn
    nbnLogin(username = 'tom_test', password = 'HelloWorld')
    
    # Create grid reference replies
    repliesGR <- gridRefTweets(tweets=tweets) 
    
    # Create and make map replies
    maptwts <- mapTweets(tweets)
    
    # Create and make naturenearme replies
    nearbytwts <- nearbyNature(tweets)
    
    # Directions to nearest
    dirtwts <- directionTweets(tweets)
    
    # Record sinceID we got up to
    sinceID <- attr(tweets, 'max_sinceID')
  }
  
  # Wait for 5.5 seconds, this keeps us under the rate limit
  Sys.sleep(5.5)
  
  return(sinceID)
}