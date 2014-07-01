cmdClearCache <- function(x){
  
  cache_files <- paste('hectad_cache/',list.files('hectad_cache/'),sep='')
  unlink(cache_files)
  
  numberDeleted<- length(cache_files)
  
  dm_resp <- try(dmSend(text=paste(numberDeleted, 'cached hectad files deleted'), user = 'TomAugust85'), silent = TRUE)
  if(class(dm_resp) == 'try-error'){
    cat('Trying sending clear cache dm again\n')
    Sys.sleep(10)
    dm_resp <-  try(dmSend(text=paste(numberDeleted, 'cached hectad files deleted'), user = 'TomAugust85'), silent = TRUE)
  }
    
}