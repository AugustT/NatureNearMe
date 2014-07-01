directionTweets <- function(tweets = NULL){
  
  load("twitteR_credentials") # load credentials
  if(!registerTwitterOAuth(Cred)) stop('login failed') # Check login
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  
  # Subset tweets
  tweets <- tweets[grepl('#directions', tolower(tweets$text)),]
  
  if(!is.null(tweets)){
  
  if(nrow(tweets) == 0){
    
    tweets <- NULL
    
  } else {
    
    tweets$replyText <- NA
    tweets$query <- NA
    
    for(i in 1:nrow(tweets)){  
      
      # get the species name out if given
      tweets$query[i] <- substr(tweets$text[i], 1, min(regexpr('#',tweets$text[i]))-1)
      tweets$query[i] <- trim(gsub('@naturenearme', '', tolower(tweets$query[i])))
      
      if(is.na(tweets$longitude[i]) | is.na(tweets$latitude[i])){
        
        tweets$replyText[i] <- "Your tweet didn't have any location information, ensure you have turned on this feature on your mobile device" 
        
      } else if(nchar(tweets$query[i]) == 0){ # no species given
      
        tweets$replyText[i] <- "Please let me know what species you would like directions to" 
        
      } else {
            
        # Get hectad data for lat long
        fullGR <- gps_latlon2gr(as.numeric(tweets$latitude[i]), as.numeric(tweets$longitude[i]), out_projection = "OSGB", return_type = "both")
        hectad <- reformat_gr(fullGR$GRIDREF, prec_out = 10000)  

          # Is data for this hectad in the cache?
          if(hectad %in% list.files('hectad_cache')){
            load(paste('hectad_cache/', hectad, sep=''))
          } else {
            
            # Perform a search using the hectad data
            occ <- try(getOccurrences(gridRef = hectad, startYear= 2008, silent=TRUE, acceptTandC=TRUE, latLong=TRUE), silent = TRUE)
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
              
            # Get the pTVK for this species
            query_resp <- try(getTVKQuery(tweets$query[i], species_only=TRUE, top=TRUE), silent = TRUE)
            if(!is.null(query_resp)){
              if(attr(query_resp, 'class') == 'try-error') query_resp <- NULL
            }
            if(is.null(query_resp)){
              
              tweets$replyText[i] <- paste("I couldn't find a species match for", tweets$query[i], sep = ' ')
              
            } else {
              
              # Get just records at 100m or better
              occ <- occ[occ$resolution %in% c('100m','10m','1m'),]
              
              # Get those for the species of interest
              occ <- occ[occ$pTaxonVersionKey %in% query_resp$ptaxonVersionKey,]
              
              if(nrow(occ) > 0){
                                
                # Calculate the distances
                distances <- spDistsN1(as.matrix(occ[,c('longitude','latitude')]), pt=c(as.numeric(tweets$longitude[i]),as.numeric(tweets$latitude[i])),longlat=T)
                      
                # Get the record at the shortest distance
                # Take the first row if we have a draw
                occ <- occ[grepl(min(distances),distances),][1,]
                
                if(nrow(occ) != 1) stop('shortest distance occ table does not have 1 row')
                
                # Create the google URL
                gurl <- paste('https://maps.google.co.uk/maps?saddr=',
                              round(as.numeric(tweets$latitude[i]),digits=6), ',', round(as.numeric(tweets$longitude[i]),digits=6),
                              '&daddr=',
                              round(occ$latitude,digits=6), ',', round(occ$longitude,digits=6),
                              '&t=m', sep='')
                                
                tweets$replyText[i] <- paste('Here are your directions', gurl) 
                                
              } else{ # if we dont have any high resolution records of this species
                
                tweets$replyText[i] <- paste('There are not any pin point records of',
                                             tweets$query[i],
                                             'nearby, sorry') 
                
              }              
                          
            }   
        } else {
        
          tweets$replyText[i] <- paste('Either there is no data for your location or our servers are having problems!') 
        
        } 
      }
    }
    
    for(i in 1:nrow(tweets)){
      
      error <- try(updateStatus(text = paste(paste("@", tweets$screenName[i], sep = ''), tweets$replyText[i]), inReplyTo=tweets$id[i]), silent = TRUE)

      if(class(error) == 'try-error'){
          cat('Trying directions again\n')
          Sys.sleep(10)
          error <- try(updateStatus(text = paste(paste("@", tweets$screenName[i], sep = ''), tweets$replyText[i]), inReplyTo=tweets$id[i]), silent = TRUE)
        }        
      }
    }
    
    # Save tweets to file
    if(file.exists('natureNearMe.csv')){
      write.table(x = tweets, file = 'natureNearMe.csv', row.names = FALSE, col.names = FALSE, append = TRUE, sep=',')
    } else {
      write.table(x = tweets, file = 'natureNearMe.csv', row.names = FALSE, col.names = TRUE, append = TRUE, sep=',')
    }
    
  } #finish where we have data
}
