nearbyNature <- function(tweets = NULL){

  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  # Subset tweets
  tweets <- tweets[grepl('#naturenearme', tolower(tweets$text)),]
    
  if(nrow(tweets) == 0){
    
    tweets <- NULL
    
  } else {
    
    tweets$replyText <- NA
    tweets$query <- NA
    
    for(i in 1:nrow(tweets)){  
      
      # get the species name out if given
      tweets$query[i] <- substr(tweets$text[i], 1, min(regexpr('#',tweets$text[i]))-1)
      tweets$query[i] <- trim(gsub('@naturenearme', '', tolower(tweets$query[i])))
            
      if(!grepl(' near ', tweets$text[i]) & (is.na(tweets$longitude[i]) | is.na(tweets$latitude[i]))){
        
        tweets$replyText[i] <- "Your tweet didn't have any location information, ensure you have turned on this feature on your mobile device" 
      
      } else {
        
        location_match <- NULL
        
        if(grepl(' near ', tweets$text[i])){
          
          if(!'gazz' %in% ls()) load(file='gazz.rdata')
          in_loc <- regexpr('near ', tweets$query[i]) + attr(regexpr('near ', tweets$query[i]), "match.length")
          location <- substr(tweets$query[i], in_loc,nchar(tweets$query[i]))
          location_details <- gazz[grepl(location,tolower(gazz$name)),][1,]
          location_match <- as.character(location_details[1,1])
          hectad <- as.character(location_details[1,2])
          tweets$query[i] <- substr(tweets$query[i],1,regexpr(' near ', tweets$query[i])-1) 
          # if last letter is 's' get rid of it
          if(substr(tweets$query[i], nchar(tweets$query[i]), nchar(tweets$query[i])) == 's'){
            tweets$query[i] <- substr(tweets$query[i], 1, nchar(tweets$query[i])-1)
          }
          
        } else {
          
          # Get hectad data for lat long
          fullGR <- gps_latlon2gr(as.numeric(tweets$latitude[i]), as.numeric(tweets$longitude[i]), out_projection = "OSGB", return_type = "both")
          hectad <- reformat_gr(fullGR$GRIDREF, prec_out = 10000)  
        
        }
        
        if(is.na(hectad)){
          
          text <- paste("I couldn't find a place called", location)
          tweets$replyText[i] <- text 
          
        } else {
        
          # Is data for this hectad in the cache?
          if(hectad %in% list.files('hectad_cache')){
            load(paste('hectad_cache/', hectad, sep=''))
          } else {
            
            # Perform a search using the hectad data
            occ <- try(getOccurrences(gridRef = hectad, startYear= 2008, silent=TRUE, acceptTandC=TRUE), silent = TRUE)
            if(!is.null(occ)){
              if(attr(occ, 'class') == "try-error"){
                occ <- NULL  
              } 
            }
            if(!is.null(occ)){
              save(occ, file = paste('hectad_cache/', hectad, sep=''))
            }
            
          }
          
          if(!is.null(occ)){
            
            # Switch depending on if a species is given        
            if(tweets$query[i]==''){ # if no species is given
              
              # Generate random information based on occurrence data
              reply_text <- try(randInfo(occ, location = location_match), silent = TRUE)
              if(class(reply_text) == "try-error"){
                tweets$replyText[i] <- 'Oops, looks like the server is having problems, please try again in a minute'
              
              } else {
                tweets$replyText[i] <- reply_text
              }
              
            } else {
              
              # Get the pTVK for this species
              query_resp <- try(getTVKQuery(tweets$query[i], species_only=TRUE, top=TRUE), silent = TRUE)
              if(!is.null(query_resp)){
                 if(attr(query_resp, 'class') == 'try-error') query_resp <- NULL
              }
              if(is.null(query_resp)){
                
                tweets$replyText[i] <- paste("I couldn't find a species match for", tweets$query[i], sep = ' ')
                
              } else {
                
                # Generate random information based on occurrence data
                reply_text <- try(randInfo(occ, TVK = query_resp$ptaxonVersionKey, sp_name = query_resp$searchMatchTitle, location = location_match), silent = TRUE)
                if(class(reply_text) == "try-error"){
                  tweets$replyText[i] <- 'Oops, looks like the server is having problems, please try again in a minute'
                } else {
                  tweets$replyText[i] <- reply_text
                }
                
              }
              
            }  
            
          } else {
            tweets$replyText[i] <- 'Oops, either the server is having problems, or there is no data for your area'
          }
        
        }   
      } 
    } #finish rows
    
    for(i in 1:nrow(tweets)){
      #sink(file='dump')
      error <- try(updateStatus(text = paste("@", tweets$screenName[i], ' ', tweets$replyText[i], sep = ''),
                                inReplyTo=tweets$id[i]), silent = TRUE)   
      
#       if(tweets$screenName[i] %in% followers){
#         dm_resp <- try(dmSend(text=tweets$replyText[i], user = tweets$screenName[i]), silent = TRUE)
#       } else {
#         error <- try(updateStatus(text = paste("@", tweets$screenName[i], ' Responses to #NatureNearMe are sent via direct message, please follow @NatureNearMe and then send your message again', sep = ''), inReplyTo=tweets$id[i]), silent = TRUE)
#         tweets$replyText[i] <- 'Responses to #NatureNearMe are sent via direct message, please follow @NatureNearMe and then send your message again'
#         if(class(error) == 'try-error'){
#           cat('Trying naturenearme again\n')
#           Sys.sleep(10)
#           error<- try(updateStatus(text = paste("@", tweets$screenName[i], ' Responses to #NatureNearMe are sent via direct message, please follow @NatureNearMe and then send your message again', sep = ''), inReplyTo=tweets$id[i]), silent = TRUE)        
#         }        
#      }
      #sink()
    }
    
    # Save tweets to file
    if(file.exists('natureNearMe.csv')){
      write.table(x = tweets, file = 'natureNearMe.csv', row.names = FALSE, col.names = FALSE, append = TRUE, sep=',')
    } else {
      write.table(x = tweets, file = 'natureNearMe.csv', row.names = FALSE, col.names = TRUE, append = TRUE, sep=',')
    }
    
  } #finish where we have data
}

