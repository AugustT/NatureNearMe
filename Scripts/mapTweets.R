mapTweets <- function(tweets = NULL){
  
  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  tweets <- tweets[grepl('#map', tolower(tweets$text)),]
    
  if(nrow(tweets) == 0){
    
    tweets <- NULL
    
  } else {
    
    tweets$replyText <- NA
    
    tweets$query <- substr(tweets$text, 1, min(regexpr('#',tweets$text))-1)
    tweets$query <- trim(gsub('@naturenearme', '', tolower(tweets$query)))
    tweets$ptaxonVersionKey <- NA
    tweets$searchMatchTitle <- NA
      
    # use nbn to get a pTVK for each query
    for(i in 1:nrow(tweets)){
      query_resp <- try(getTVKQuery(tweets$query[i], species_only=TRUE, top=TRUE), silent=TRUE)
      if(!is.null(query_resp)) if(attr(query_resp, 'class') == 'try-error') query_resp <- NULL      
      if(!is.null(query_resp)){
        tweets$ptaxonVersionKey[i] <- query_resp$ptaxonVersionKey
        tweets$searchMatchTitle[i] <- query_resp$searchMatchTitle
      }
    }
    
    # Create URL to view
    tweets$mapURL <- paste('https://data.nbn.org.uk/imt/?mode=SPECIES&species=', tweets$ptaxonVersionKey, sep = '')
    tweets$maptext <- NA
    
    for(i in 1:nrow(tweets)){  
      
      if(is.na(tweets$ptaxonVersionKey[i])){
        tweets$maptext[i] <- paste("@", tweets$screenName[i], " I couldn't find a species match for ", tweets$query[i], sep = '')
      } else {
        tweets$maptext[i] <- paste("@", tweets$screenName[i], ' Here is the map for ', tweets$searchMatchTitle[i], ' ', tweets$mapURL[i], sep='')
      } 
      
      error <- try(updateStatus(text = tweets$maptext[i], inReplyTo = tweets$id[i]), silent = TRUE)
      if(class(error) == "try-error"){
        cat('Trying mapTweets again\n'); Sys.sleep(10)
        error <- try(updateStatus(text = tweets$maptext[i], inReplyTo = tweets$id[i]), silent = TRUE)
      }  
      
    }  
    
    # Save tweets to file
    if(file.exists('maps.csv')){
      write.table(x = tweets, file = 'maps.csv', row.names = FALSE, col.names = FALSE, append = TRUE, sep=',')
    } else {
      write.table(x = tweets, file = 'maps.csv', row.names = FALSE, col.names = TRUE, append = TRUE, sep=',')
    }
  }
  
  return(tweets)
  
}