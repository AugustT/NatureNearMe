###Retrieve tweets to the account###
getTweets <- function(sinceID = NULL){

  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  UN <- '@NatureNearMe'
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  resp <<- try(searchTwitter(UN, cainfo = "cacert.pem", sinceID = sinceID), silent=TRUE)
  if(!is.null(attr(resp, 'class'))) if(attr(resp, 'class') == 'try-error') resp <- NULL
  
  if(length(resp) == 0){
    
    raw_twit <- NULL
  
  } else {
    
    raw_twit <<- try(twListToDF(resp), silent = TRUE)
    
    if(class(raw_twit) == 'try-error'){
      
      raw_twit <- NULL
      
    } else {
      
      # This function isn't great at using sinceID so filter again
      if(!is.null(sinceID)) raw_twit <- raw_twit[raw_twit$id > sinceID,]
      
      # Remove retweets
      raw_twit <- raw_twit[raw_twit$isRetweet == FALSE,]
      
      # Keep with only those that start with specified username
      raw_twit <- raw_twit[tolower(substr(raw_twit$text,1,13)) == tolower(UN),]
      
      #followers <- friendships(screen_names = raw_twit$screenName)
      
      #raw_twit$follower <- followers$followed_by[match(raw_twit$screenName, followers$screen_name)]
      
      attr(raw_twit, 'max_sinceID') <- max(raw_twit$id)
      
      if(nrow(raw_twit) == 0) raw_twit = NULL 
      
    }
    
  }
    
  return(raw_twit)

}