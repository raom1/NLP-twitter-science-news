#This Rscript is a twitter listener. It will listen to the live twitter stream and save tweets
#that match the given search criteria. This will be the workhorse for capturing tweets.

library(twitteR)
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ROAuth)

#Run the first part only once.
#Scroll down to where it says START HERE if you have already run the first part.

consumer_key <- "<Your Customer Key>"
consumer_secret <- "<Your Customer Secret>"
access_token <- "<Your Access Token>"
access_secret <- "<Your Access Secret>"

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

my_oauth <- OAuthFactory$new(consumerKey = consumer_key,
                             consumerSecret = consumer_secret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

#Once you run this part a browser window will popup with a code you need to paste 
#into the console
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

#This is so that you don't have to run the above code each time you start a new Rsession.
save(my_oauth, file = "my_oauth.Rdata")

#START HERE

#A "handshake" was done previously for authentication with the app created on twitter.
#The credentials were saved in the Rdata file here that is being loaded. 

load("my_oauth.Rdata")

#Setup OAuth for current session
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Assigning twitter handles to variables. Possibly unnecessary but format becomes uniform.

nature <- "nature"
science <- "sciencemagazine"
cell <- "CellCellPress"
nejm <- "NEJM"
lancet <- "TheLancet"
jama <- "JAMA_current"
sciam <- "sciam"
natgeo <- "NatGeo"
wired <- "WIRED"
bbc <- "BBCScienceNews"
nyt <- "NYTScience"
npr <- "nprscience"
tyson <- "neiltyson"
rosling <- "HansRosling"
goldacre <- "bengoldacre"
nasa <- "NASA"
nih <- "NIH"
smith <- "smithsonian"

#Vector with all the handle variables. Will be passed into listener. 
#If new handles passed then modify this list.
usernames <- c(nature, 
               science, 
               cell, 
               nejm, 
               lancet, 
               jama, 
               sciam, 
               natgeo, 
               wired, 
               bbc, 
               nyt, 
               npr, 
               tyson, 
               rosling, 
               goldacre, 
               nasa,
               nih,
               smith)

#Function lookupUsers is part of twitteR package. 
#Takes users names and returns their metadata.
#We only are taking the ID but other things can be used as well.
#Check out df to see what's available.
username_df <- twListToDF(lookupUsers(usernames))

#This is the listener. 
#IMPORTANT! If the file already exists then new data will be appended to it.
#It will be saved as json so keep that extension.
#If using "follow" command need to enter user IDs. Can be as string or numeric.
#Use ?filterStream to see other find other args.
filterStream(file.name = "tweet_test.json", 
             follow = username_df$id, 
             timeout = 60,
             #tweets = 10,
             oauth = my_oauth)

#Parse json of tweets into data frame. 
stream_tweets_df <- parseTweets("tweet_test.json", simplify = FALSE)
