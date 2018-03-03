#### FUNCTIONS FOR N-GRAMS ####

clean_text_no_tm <- function(text_col) {
  tweet_text <- str_replace_all(text_col, "[^[:graph:]]", " ") %>% 
    tolower() %>% 
    sapply(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl = T)) %>% 
    sapply(function(x) gsub("rt", "", x)) %>% 
    sapply(function(x) gsub("@\\w+", "", x)) %>% 
    unname()
  tibble(tweet_number = 1:length(tweet_text), text = tweet_text) %>% 
    unnest_tokens(word, text) %>% 
    filter(!word %in% stop_words$word)
}

#### FUNCTIONS FOR WORDCLOUDS ####

make_cluster_dataframe <- function(data_matrix, number_of_clusters) {
  k_means_result <- kmeans(data_matrix, number_of_clusters)
  centers <- data.frame(t(k_means_result$centers))
  colnames(centers) <- c(1:number_of_clusters)
  centers$cluster <- as.numeric(colnames(centers)[max.col(centers,ties.method = "first")])
  centers$terms <- rownames(centers)
  centers$freq <- rowSums(as.matrix(t(data_matrix)))
  return(centers)
}

make_word_cloud <- function(cluster_number, df) {
  center <- filter(df, cluster == cluster_number)
  wordcloud(center$terms, center$freq, max.words = 15, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))
}

#### FUNCTIONS FOR TEXT CLEANING ####

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
}