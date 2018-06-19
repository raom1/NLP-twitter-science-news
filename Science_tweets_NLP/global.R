library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(wordcloud)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(plyr)

load("./Data/user_search_full.Rdata")
load("./Data/username_df.Rdata")

tweets_followers <- left_join(user_search_full,
                              username_df[,c("followersCount", "screenName")],
                              by = "screenName")

tweets_followers$norm_popular <- ((tweets_followers$favoriteCount + tweets_followers$retweetCount) / tweets_followers$followersCount) * 1000000

popular_arrange <- tweets_followers %>% 
  filter(isRetweet == FALSE) %>%
  arrange(desc(norm_popular))

popular_cutoff <- round(nrow(popular_arrange)*0.1)

top_10_percent <- popular_arrange[1:popular_cutoff,]

bottom_90_percent <- anti_join(popular_arrange, top_10_percent)

my_stopwords <- c(stop_words$word, "aicle", "eah", "amp", "ou", "won't", "i'd", "mes", "ineff", "rcts", "sugg", "bn", "linehan", "al", "that's", "guo", "al", "fo", "liu", "it's", "mus")

# This function will take in a dataframe of data from twitter,
# extract the text column, clean it, and put it into a new df
# where each row is one word from a tweet
clean_text_no_tm <- function(df) {
  tweet_text <- str_replace_all(df$text, "[^[:graph:]]", " ") %>% 
    tolower() %>% 
    sapply(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl = T)) %>% 
    sapply(function(x) gsub("[[:digit:]]", "", x)) %>% 
    sapply(function(x) gsub("@\\w+", "", x)) %>% 
    unname()
  week_bins <- seq(as.POSIXct(as.character(min(popular_arrange$created))),
                   as.POSIXct(as.character(max(popular_arrange$created))),
                   by = "7 days")
  tibble(tweet_number = df$id,
         screen_name = df$screenName,
         text = tweet_text,
         created = df$created,
         week_number = cut(created,
                           breaks = c(week_bins, Inf),
                           labels = c(1:length(week_bins)))) %>% 
    unnest_tokens(word, text) %>% 
    filter(!word %in% my_stopwords)
}

popular_text_date <- clean_text_no_tm(top_10_percent)#[1:10000, ]

unpopular_text_date <- clean_text_no_tm(bottom_90_percent)#[1:10000, ]
