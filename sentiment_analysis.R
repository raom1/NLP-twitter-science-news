library(tidytext)
library(tidyverse)
library(ggplot2)
library(qdap)
library(tm)
library(igraph)
library(ggraph)
library(widyr)

load("Documents/Data Science Bootcamp/R/Twitter_R/user_search_020618.Rdata")

#View(user_search_df)

tweet_text <- user_search_df$text

#Clean the text

tweet_text <- str_replace_all(tweet_text, "[^[:graph:]]", " ")

tweet_text <- tolower(tweet_text)

tweet_text <- sapply(tweet_text, function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))

tweet_text <- sapply(tweet_text, function(x) gsub("rt", "", x))

tweet_text <- sapply(tweet_text, function(x) gsub("@\\w+", "", x))

#tweet_text <- sapply(tweet_text, function(x) gsub('[[:digit:]]+', '', x))

tweet_text <- unname(tweet_text)

tweet_tbl <- tibble(tweet_number = 1:length(tweet_text), text = tweet_text)

unnest_text <- tweet_tbl %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

# SENTIMENT ANALYSIS

#What are the most common sentiments across tweets? Use nrc sentiment set (assign to emotion group)
unnest_text %>%
  right_join(get_sentiments("nrc")) %>%
  #filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

#What is the most common word with given sentiment? Use bing sentiment set (binary pos or neg)
unnest_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#How does the sentiment of tweets change overtime? Tweets are in chronological order already.
#Use afinn sentiment set (scale of -5 to 5 of how positive it is)
unnest_text %>%
  group_by(tweet_number) %>% 
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(score)) %>% 
  ggplot(aes(x = tweet_number, y = sentiment)) +
  geom_bar(stat = "identity")


# TF-IDF...just if you ever need it
tweet_tfidf <- unnest_text %>%
  count(tweet_number, word, sort = F) %>%
  ungroup()

tweet_tfidf <- tweet_tfidf %>%
  bind_tf_idf(word, tweet_number, n)

tweet_tfidf %>%
  arrange(desc(tf_idf))


#N-GRAMS

# stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
# stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
# tweet_ngram <- str_replace_all(tweet_text, stopwords_regex, "")

unnest_bigram <- tweet_tbl %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

count(unnest_bigram, bigram, sort = T)

unnest_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

bigram_tf_idf <- unnest_bigram %>%
  count(tweet_number, bigram, sort = TRUE) %>%
  bind_tf_idf(bigram, tweet_number, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

bigram_graph <- unnest_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
#  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) #+
  #theme_void()

text_graph <- unnest_text %>%
  filter(!word %in% stop_words$word,
         !word == "amp") %>% 
  count(word, sort = TRUE) %>%
  #  filter(n > 20) %>%
  graph_from_data_frame()
text_graph

ggraph(text_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


unnest_text

pairwise_count(unnest_text, word, tweet_number, sort = TRUE)

word_cor <- unnest_text %>%
    group_by(word) %>%
    #filter(n() >= 20) %>%
    pairwise_cor(word, tweet_number) %>%
    filter(!is.na(correlation))

word_cor %>%
  arrange(desc(correlation))

word_cor %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
