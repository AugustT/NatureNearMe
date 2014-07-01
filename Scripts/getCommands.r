getCommands <- function(username = '@tomaugust85', sinceID = NULL){
  
  # get direct messages
  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  dms <- try(dmGet(sinceID = sinceID), silent = TRUE)
  
  if(!is.null(attr(dms,'class'))){
    if(class(dms) == 'try-error'){
      dms <- NULL    
    }
  }
  
  if(length(dms) > 0){
  
    text <- NULL
    sender <- NULL
    
    for(i in 1:length(dms)){
      text <- c(text, dms[[i]]$text)
      sender <- c(sender, as.character((dms[[i]]$sender)$name))
    }
    
    # Get dms from me only
    dm <- data.frame(text = text, sender = sender)
    print(dm)
    dm <- dm[tolower(dm$sender) == 'tom august']
    
    # look for summary request
    if(TRUE %in% grepl('#summary', dm$text)){
      cmdSummary()
    }
    # look for clearcache request
    if(TRUE %in% grepl('#clearcache', dm$text)){
      cmdClearCache()
    }
    
    sinceID <- dms[[1]]$id
    
  } 
  
  return(sinceID)

}