library(tm)
library(stringr)


#load("~/Documents/Data Science Bootcamp/R/Twitter_R/user_search_020618.Rdata")



clean_tweet_text <- function(df) {
  no_graph <- data.frame(text = str_replace_all(df$text, "[^[:graph:]]", " ")) 
  cleaned <- VCorpus(VectorSource(as.vector(no_graph$text))) %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+",
                                                               "",
                                                               x,
                                                               perl=T))) %>% 
    tm_map(content_transformer(function(x) gsub("rt", "", x))) %>% 
    tm_map(content_transformer(function(x) gsub("@\\w+", "", x))) %>% 
    tm_map(removeWords, words = c(stopwords("english"), "aicle", "amp", "can", "new", "one", "get", "next", "now", "keep", "like", "made", "just", "big", "good", "may", "need", "time", "best", "ever", "way", "eah", "might", "see", "well", "getting", "ready", "two", "better", "cool", "many", "much", "never", "take", "got", "really", "back", "get", "great", "sure", "yes", "come", "know", "make", "says", "meet", "seen", "thing", "types", "use", "will")) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers)
  #tm_map(cleaned, stemDocument) #%>% 
    #tm_map(stemCompletion(dictionary = cleaned))
}

#search_corpus <- clean_tweet_text(user_search_df)

# #The below parts are the individual commands that comprise the function
#
# #Removes special characters (emojis) or other things that make cleaning text difficult later
#
# user_search_df$text <- str_replace_all(user_search_df$text, "[^[:graph:]]", " ")
#
# VCorpus(VectorSource(as.vector(user_search_df$text)))
# 
# user_search_df$text <- str_replace_all(user_search_df$text,"[^[:graph:]]", " ")
# 
# search_corpus <- VCorpus(VectorSource(as.vector(user_search_df$text)))
# 
# #inspect(search_corpus[[1]])
# 
# search_corpus <- tm_map(search_corpus, content_transformer(tolower))
# 
# removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
# 
# # Replace blank space (“rt”)
# remove_rt <- content_transformer(function(x) gsub("rt", "", x))
# 
# # Replace @UserName
# remove_tag <- content_transformer(function(x) gsub("@\\w+", "", x))
# 
# search_corpus <- tm_map(search_corpus, removeURL)
# 
# search_corpus <- tm_map(search_corpus, remove_rt)
# 
# search_corpus <- tm_map(search_corpus, remove_tag)
# 
# #Adding additional args (preserve_intra_word_contractions and preserve_intra_word_dashes) cause error.
# #Error in UseMethod("removePunctuation") : no applicable method for 'removePunctuation' applied to an object of class "logical"
# #The args are logicals, perhaps why error is thrown but how to get around that?
# 
# #my_stop_words <- c("can", "new", "one", "get", "next", "now", "keep", "like", "made", "just", "big", "youre", "good", "may", "need", "time", "best", "ever", "way", "eah", "might", "see", "well", "dont", "getting", "ready", "two", "better", "cool", "many", "much", "never", "take", "got", "really", "back", "get", "great", "sure", "yes", "come", "know", "make", "says", "theres", "lets", "meet", "seen", "thing", "types", "use", "youve")
# 
# #serach_corpus <- tm_map(search_corpus, removeWords, words = my_stop_words)
# 
# search_corpus <- tm_map(search_corpus, removeWords, words = c("amp", "can", "new", "one", "get", "next", "now", "keep", "like", "made", "just", "big", "youre", "good", "may", "need", "time", "best", "ever", "way", "eah", "might", "see", "well", "dont", "getting", "ready", "two", "better", "cool", "many", "much", "never", "take", "got", "really", "back", "get", "great", "sure", "yes", "come", "know", "make", "says", "theres", "lets", "meet", "seen", "thing", "types", "use", "youve"))
# 
# search_corpus <- tm_map(search_corpus, removeWords, stopwords("english"))
# 
# search_corpus <- tm_map(search_corpus, removePunctuation)
# 
# search_corpus <- tm_map(search_corpus, removeNumbers)
# 
# #search_corpus <- tm_map(search_corpus, stripWhitespace)
# 
# search_corpus <- tm_map(search_corpus, stemDocument)
# 
# #inspect(search_corpus[[1]])
#
# save(search_corpus, file = "search_corpus.Rdata")

