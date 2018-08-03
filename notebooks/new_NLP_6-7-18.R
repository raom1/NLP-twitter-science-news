library(tm)
library(tokenizers)
library(stopwords)
library(apcluster)

View(user_search_full)

my_stopwords <- c(stop_words$word, "aicle", "eah", "amp", "ou", "won't", "i'd", "mes", "ineff", "rcts", "sugg", "bn", "linehan", "al", "that's", "guo", "al", "fo", "liu", "it's", "mus")

full_text <- user_search_full$text

full_text_clean <- str_replace_all(full_text, "[^[:graph:]]", " ") %>% 
  tolower() %>% 
  sapply(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl = T)) %>% 
  sapply(function(x) gsub("[[:digit:]]", "", x)) %>% 
  sapply(function(x) gsub("@\\w+", "", x)) %>% 
  sapply(function(x) gsub("#\\w+", "", x)) %>% 
  sapply(function(x) gsub("[[:punct:] ]+", " ", x)) %>% 
  sapply(function(x) removeWords(x, my_stopwords)) %>% 
  trimws("both") %>% 
  strsplit("\\s *")

tw_1 <- full_text_clean[[1]]
tw_2 <- full_text_clean[[2]]

tweets_short_list <- c(full_text_clean[1:10000])

calculate_similarity <- function(tw_1, tw_2) {
  similarity_score <- length(intersect(tw_1, tw_2))/((length(tw_1)+length(tw_2))/2)
  return(similarity_score)
}

tweets_short_list[[4]]

tweets_short_list[[5]]

calculate_similarity(tweets_short_list[[2]], tweets_short_list[[2]])

comparison <-  1
tweet_position <- 1
l <- length(full_text_clean)
rows <- list(l)
start_time <- Sys.time()
while (tweet_position <= l) {
  cols <- c(l)
  while (comparison <= l) {
    similarity_score <- calculate_similarity(full_text_clean[[tweet_position]], full_text_clean[[comparison]])
    cols[comparison] <- similarity_score
    comparison <- comparison + 1
  }
  rows[[tweet_position]] <- cols
  tweet_position = tweet_position + 1
  comparison = 1
}
end_time <- Sys.time()
end_time-start_time
sim_mat <- do.call(rbind, rows)
View(sim_mat)

save(sim_mat, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/sim_mat.Rdata")
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/sim_mat.Rdata")

ap_full_text <- apcluster(sim_mat)
