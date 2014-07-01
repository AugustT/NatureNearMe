rm(list=ls())

# Load the required R libraries

library(twitteR)
library(ROAuth)
library(RCurl)

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="cacert.pem")

# Set constant requestURL
requestURL <- "https://api.twitter.com/oauth/request_token"
# Set constant accessURL
accessURL <- "https://api.twitter.com/oauth/access_token"
# Set constant authURL
authURL <- "https://api.twitter.com/oauth/authorize"

consumerKey <- "cx26FPnSjIVFywT8xN9Q"
consumerSecret <- "go6xOFCvTr6FiDPyVcRUYINwqrvaqkcceZnN6Tcs"

Cred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

registerTwitterOAuth(Cred)

save(list="Cred", file="twitteR_credentials")

# #Development version of twitter uses a different function for this:
# I'm waiting for documentation to see how it is used.
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret, 
#  credentials_file = NULL) 