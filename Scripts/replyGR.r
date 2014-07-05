## send replies regarding grid references
replyGR <- function(repliesGR=NULL){
    
  if(!is.null(repliesGR)){
    # Save replies
    if(file.exists('gridref.csv')){
      write.table(x = repliesGR, file = 'gridref.csv', row.names = FALSE, col.names = FALSE, append = TRUE, sep=',')
    } else {
      write.table(x = repliesGR, file = 'gridref.csv', row.names = FALSE, col.names = TRUE, append = TRUE, sep=',')
    }
    for(i in 1:nrow(repliesGR)){
      #sink(file='dump')
      
      error <- try(updateStatus(text = paste("@", tweets$screenName[i], ' ', repliesGR$replyText[i], sep = ''),
                                inReplyTo=repliesGR$id[i]), silent = TRUE)      
      
# This code was previously used to send DMs      
#       if(repliesGR$screenName[i] %in% followers){
#         dm_resp <- try(dmSend(text=repliesGR$replyText[i], user = repliesGR$screenName[i]), silent = TRUE)
#       } else {
#         error <- try(updateStatus(text = paste("@", tweets$screenName[i], ' Responses to #gridref are sent via direct message, please follow @NatureNearMe, then send your message again', sep = ''), inReplyTo=repliesGR$id[i]), silent = TRUE)
#         if(class(error) == 'try-error') error <- try(updateStatus(text = paste("@", tweets$screenName[i], ' like I said before, you need to follow @NatureNearMe to get responses :)', sep = ''), inReplyTo=repliesGR$id[i]), silent = TRUE)        
#       }
      #sink()
      #print(repliesGR[i,])
    }
  } else {
    #print('No gridRef replies made')
  }
}