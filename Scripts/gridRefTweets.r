###Get grid reference requests###
#Returns text and inReplyTo##
gridRefTweets <- function(tweets = NULL){
  
  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  tweets <- tweets[grepl('#gridref', tolower(tweets$text)),]
  
  if(nrow(tweets) == 0){
    
    tweets <- NULL
  
  } else {
  
    tweets$replyText <- NA
  
    for(i in 1:nrow(tweets)){  

      if(is.na(tweets$longitude[i]) | is.na(tweets$latitude[i])){
        tweets$replyText[i] <- "Your tweet didn't have any location information, ensure you have turned on this feature on your mobile device" 
      } else {
        fullGR <- gps_latlon2gr(as.numeric(tweets$latitude[i]), as.numeric(tweets$longitude[i]), out_projection = "OSGB", return_type = "both")
        monad <- reformat_gr(fullGR$GRIDREF, prec_out = 1000)
        hectad <- reformat_gr(fullGR$GRIDREF, prec_out = 10000)
        tweets$replyText[i] <- paste("Your location is ", fullGR$GRIDREF, ", easting:", fullGR$EASTING, " northing:", fullGR$NORTHING,
                                  ", latitude:", tweets$latitude[i], " longitude:", tweets$longitude[i], sep='')
      }  
    }
    tweets <- tweets[,c('replyText', 'id', 'screenName', 'created')]
    
    # Reply to gridRefTweets
    replyGR(tweets)
  }
  
  return(tweets)
  
}