getFollowers <- function(user=NULL){
  
  if(is.null(user)) stop('user must be given')
  
  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
    
  followers_temp <- getUser(user)$getFollowers()
  test <- twListToDF(followers_temp)
  followers <- test$screenName
  myFriendships <- friendships(followers)
  
  return(myFriendships)
  
}