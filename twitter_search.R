#This Rscript is for searching twitter for tweets based on specific search criteria.
#It can only search over the past 10 days but there is no limit on the number of tweets
#(that I've encountered yet)

library(twitteR)

consumer_key <- "<Your Customer Key>"
consumer_secret <- "<Your Customer Secret>"
access_token <- "<Your Access Token>"
access_secret <- "<Your Access Secret>"

#Need to run at the beginning of every R session
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#This function can be used iteratively to retreive as many tweets as desired. 
#Tweets will be added to the end of an existing data frame and 
#each execution of the function will add the next batch.
#Tweets are retrieved in reverse chronological order, based on tweet id.
#No argument for the number of tweets has been specified. 
#Default is 25 but can be increased by specifying the "n" arg.

# search_tweets <- function(searchString, df) {
#   search_df <- twListToDF(searchTwitter(searchString = searchString,
#                                         maxID = tail(df$id, 1)))
#   search_df <- search_df[-1,]
#   return(rbind(df, search_df))
# }

#Basic function to search for tweets from a user. 
#Default search criteria for searchTwitter function.
#Pass in twitter handle in quotes.
#Will return first 1000 tweets in reverse chronological order.
#Based on previous experience there will not be more than 
#1000 tweets in a 10 day window for a single user.
#An error will print indicating that fewer than the requested number of tweets were retrieved.
#This is ok and the tweets will be safe and sound.
#If no error occurs then more there are more than 1000 tweets within the last 10 days
#If more tweets are desired use the above function and run until error prints 
#indicating fewer results retrieved than requested.
#This function is actually really unnecessary, can just use searchTwitter func with a few extra key strokes.
search_twitter <- function(handle) {
  return(twListToDF(searchTwitter(searchString = paste0("from:", handle), n = 1000)))
}

nyt_df <- search_twitter("NYTScience")
nature_df <- search_twitter("nature")
natgeo_df <- search_twitter("NatGeo")
nejm_df <- search_twitter("NEJM")
cell_df <- search_twitter("CellCellPress")
bbc_df <- search_twitter("BBCScienceNews")
goldacre_df <- search_twitter("bengoldacre")
jama_df <- search_twitter("JAMA_current")
lancet_df <- search_twitter("TheLancet")
npr_df <- search_twitter("nprscience")
rosling_df <- search_twitter("HansRosling") #didn't return anything when searched on 2-6-18
sciam_df <- search_twitter("sciam")
science_df <- search_twitter("sciencemagazine")
tyson_df <- search_twitter("neiltyson")
wired_df <- search_twitter("WIRED")
nasa_df <- search_twitter("NASA")
nih_df <- search_twitter("NIH")
smith_df <- search_twitter("smithsonian")

user_search_df_2 <- rbind(bbc_df, 
                          cell_df, 
                          goldacre_df, 
                          jama_df, 
                          lancet_df, 
                          nasa_df, 
                          natgeo_df, 
                          nature_df, 
                          nejm_df, 
                          npr_df, 
                          nyt_df, 
                          sciam_df, 
                          science_df, 
                          tyson_df, 
                          wired_df,
                          nih_df,
                          smith_df)

#user_search_df_3 <- rbind(nih_df, smith_df)

#save(user_search_df_3, file = "user_search_021418.Rdata")

save(user_search_df_2, file = "user_search_020318.Rdata")


#Just a little bit of exploratory data analysis
library(ggplot2, tidyverse)

user_search_df %>% 
  ggplot(aes(x = retweetCount, y = favoriteCount, color = screenName)) +
  geom_point()


user_search_df %>% 
  ggplot(aes(x = id, y = retweetCount)) +
  geom_bar(stat = "identity")

median(user_search_df$retweetCount)
mean(user_search_df$retweetCount)

user_search_df %>% 
  filter(retweetCount >= median(user_search_df$retweetCount)) %>% 
  ggplot(aes(x = retweetCount)) +
  geom_histogram(bins = 50)