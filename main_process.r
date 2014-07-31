# Version 0.1.1 05/06/2014
rm(list=ls())

# If we exit this script send me a message
on.exit(try(dmSend(text='@NatureNearMe has quit', user = 'TomAugust85'), silent = TRUE))

# loads packages and scripts and gets sinceIDs
source('Scripts/setup.r')
sIDs <- setup()
sinceID <- sIDs$sinceID
sinceID_cmd <- sIDs$sinceID_cmd
datum_vars <- sIDs$datum_vars
helmert_trans_vars <- sIDs$helmert_trans_vars

# Setup an infinite loop
end <- FALSE
while(end == FALSE){
  
  for(i in 1:20){
    sinceID <- try(runInteractions(sinceID), silent = TRUE)
    if(class(sinceID) == 'try-error'){ #if we have an error record it and tell me about it
      try(dmSend(text=paste('@NatureNearMe interactions error at', Sys.time()), user = 'TomAugust85'), silent = TRUE)
      print(paste(Sys.time(), sinceID))
    }
  }
  
  # Get dm and look for commands
  cat('checking for commands\n')
  sinceID_cmd <- try(getCommands(sinceID = sinceID_cmd), silent = TRUE)
  if(class(sinceID_cmd) == 'try-error'){ #if we have an error record it and tell me about it
    try(dmSend(text=paste('@NatureNearMe commands error at', Sys.time()), user = 'TomAugust85'), silent = TRUE)
    print(paste(Sys.time(), sinceID_cmd))
  }
}