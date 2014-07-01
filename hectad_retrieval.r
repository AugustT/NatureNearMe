rm(list=ls())
library(rnbn)
source('Scripts/getAllData.r')

load('hectads.rdata')

hecsGot <- list.files('hectad_cache/')
length(hectads)
hectads <- hectads[!hectads %in% hecsGot]
hectads[1]

# replenish hectad data
getAllData(hectads=hectads, dir='hectad_cache/',startyear=2008,verbose=T)