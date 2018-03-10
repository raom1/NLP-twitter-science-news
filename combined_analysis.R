library(tidytext)
library(tidyverse)
library(ggplot2)
#library(qdap)
#library(tm)
library(igraph)
library(ggraph)
library(widyr)
library(twitteR)
#library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ROAuth)
#library(proxy)
#library(mclust)
#library(RColorBrewer)
library(wordcloud)

consumer_key <- "<Your_Counsumer_Key>"
consumer_secret <- "<Your_Consumer_Secret>"
access_token <- "<Your_Access_Token>"
access_secret <- "<Your_Access_Secret>"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

load("<Path-to-my_oauth>")

usernames <- c("nature", 
               "sciencemagazine", 
               "CellCellPress", 
               "NEJM", 
               "TheLancet", 
               "JAMA_current", 
               "sciam", 
               "NatGeo", 
               "WIRED", 
               "BBCScienceNews", 
               "NYTScience", 
               "nprscience", 
               "neiltyson", 
               "HansRosling", 
               "bengoldacre", 
               "NASA",
               "NIH",
               "smithsonian")

username_df <- twListToDF(lookupUsers(usernames))

source("~/Documents/GIT/DS1/NLP-twitter-science-news/functions.R")

set.seed(123)

#Load full text data frame
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_030618.Rdata")
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_030918.Rdata")

tweets_followers <- left_join(user_search_full_030918,
                              username_df[,c("followersCount", "screenName")],
                              by = "screenName")

tweets_followers$norm_popular <- ((tweets_followers$favoriteCount + tweets_followers$retweetCount) / tweets_followers$followersCount) * 1000000

# TAKE OUT RETWEETS?? I think yes.

tweets_followers %>% 
  filter(isRetweet == FALSE) %>% 
  ggplot(aes(x = reorder(id, -norm_popular), y = norm_popular)) +
  geom_bar(stat = "identity")

popular_arrange <- tweets_followers %>% 
  filter(isRetweet == FALSE) %>%
  arrange(desc(norm_popular))

#save(popular_arrange, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_arrange.Rdata")

load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_arrange.Rdata")

popular_cutoff <- round(nrow(popular_arrange)*0.1)

top_10_percent <- popular_arrange[1:popular_cutoff,]

bottom_90_percent <- anti_join(popular_arrange, top_10_percent)

popular_arrange %>% 
  ggplot(aes(x = reorder(id, -norm_popular), y = log10(norm_popular), fill = id %in% top_10_percent$id)) +
  geom_bar(stat = "identity")



popular_corpus <- clean_tweet_text(top_10_percent)

unpopular_corpus <- clean_tweet_text(bottom_90_percent)

popular_dtm <- DocumentTermMatrix(popular_corpus)

unpopular_dtm <- DocumentTermMatrix(unpopular_corpus)


#What words appear more than 50 times
findFreqTerms(popular_dtm, 15)

findFreqTerms(unpopular_dtm, 80)

#What words are associated with selected word more than 30% of the time
findAssocs(popular_dtm, "research", 0.3)

findAssocs(unpopular_dtm, "research", 0.2)

# Compare freq terms
# Do anti-join to find words that are unique to each set.
# Create word cloud


popular_mat <- as.matrix(removeSparseTerms(popular_dtm, sparse = 0.99))

distMatrix_popular <- dist(scale(popular_mat), method = "cosine")

fit_popular <- hclust(distMatrix_popular, method="ward.D")

plot(fit_popular)


unpopular_mat <- as.matrix(removeSparseTerms(unpopular_dtm, sparse = 0.99))

distMatrix_unpopular <- dist(scale(popular_mat), method = "cosine")

fit_unpopular <- hclust(distMatrix_unpopular, method = "ward.D")

plot(fit_unpopular)


k.max <- 24

pop_wss_50_rep <- replicate(50,
                            sapply(1:k.max,
                                   function(k){kmeans(popular_mat,
                                                      k,
                                                      nstart=10,iter.max = 1000 )$tot.withinss}))

plot(1:k.max, rowMeans(pop_wss_50_rep),
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# 7


unpop_wss_50_rep <- replicate(50,
                              sapply(1:k.max,
                                     function(k){kmeans(unpopular_mat,
                                                        k,
                                                        nstart=10,
                                                        iter.max = 1000 )$tot.withinss}))

plot(1:k.max, rowMeans(unpop_wss_50_rep),
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# 7/10

# BIC
d_clust_popular <- Mclust(popular_mat, G=1:15, 
                          modelNames = mclust.options("emModelNames"))

d_clust_popular$BIC

# 1


d_clust_unpopular <- Mclust(unpopular_mat, G=1:15, 
                            modelNames = mclust.options("emModelNames"))

d_clust_unpopular$BIC

# 9


top_words_in_cluster <- function(number_of_clusters, data_matrix, number_of_words = 5) {
  k_means_result <- kmeans(data_matrix, number_of_clusters)
  for (i in 1:number_of_clusters) {
    words = names(sort(k_means_result$centers[i,], decreasing=T))[1:number_of_words]
    return(words)
  }
}


k_BCI_popular <- 1
k_BCI_unpopular <- 9
k_elbow_popular <- 7
k_elbow_unpopular <- 7

top_BCI_popular <- top_words_in_cluster(k_BCI_popular, popular_mat)
top_BCI_unpopular <- top_words_in_cluster(k_BCI_unpopular, unpopular_mat)
top_elbow_popular <- top_words_in_cluster(k_elbow_popular, popular_mat)
top_elbow_unpopular <- top_words_in_cluster(k_elbow_unpopular, unpopular_mat)

make_cluster_dataframe <- function(data_matrix, number_of_clusters) {
  k_means_result <- kmeans(data_matrix, number_of_clusters)
  centers <- data.frame(t(k_means_result$centers))
  colnames(centers) <- c(1:number_of_clusters)
  centers$cluster <- as.numeric(colnames(centers)[max.col(centers,ties.method = "first")])
  centers$terms <- rownames(centers)
  centers$freq <- rowSums(as.matrix(t(data_matrix)))
  return(centers)
}

elbow_popular <- make_cluster_dataframe(popular_mat, k_elbow_popular)
elbow_unpopular <- make_cluster_dataframe(unpopular_mat, k_elbow_unpopular)
popular <- make_cluster_dataframe(popular_mat, 1)
unpopular <- make_cluster_dataframe(unpopular_mat, 1)

save(popular, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular.Rdata")
save(unpopular, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/unpopular.Rdata")


make_word_cloud <- function(cluster_number, df) {
  center <- filter(df, cluster == cluster_number)
  wordcloud(center$terms, center$freq, max.words = 15, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))
}

make_word_cloud(1, elbow_popular)
make_word_cloud(2, elbow_popular)
make_word_cloud(3, elbow_popular)
popular_wordcloud <- make_word_cloud(1, popular)
unpopular_wordcloud <- make_word_cloud(1, unpopular)

save(popular_wordcloud, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_wordcloud.Rdata")
save(unpopular_wordcloud, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/unpopular_wordcloud.Rdata")






clean_text_no_tm <- function(text_col) {
  tweet_text <- str_replace_all(text_col, "[^[:graph:]]", " ") %>% 
    tolower() %>% 
    sapply(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl = T)) %>% 
    sapply(function(x) gsub("rt", "", x)) %>% 
    sapply(function(x) gsub("@\\w+", "", x)) %>% 
    unname()
  tibble(tweet_number = 1:length(tweet_text), text = tweet_text) %>% 
    unnest_tokens(word, text) %>% 
    filter(!word %in% stop_words$word) %>% 
    filter(!word == "aicle") %>% 
    filter(!word == "eah") %>% 
    filter(!word == "amp")
}

my_stopwords <- c(stop_words$word, "aicle", "eah", "amp", "ou", "won't", "i'd", "mes", "ineff", "rcts", "sugg", "bn", "linehan", "al", "that's", "guo", "al", "fo", "liu", "it's", "mus")

clean_text_no_tm_2 <- function(df) {
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
         text = tweet_text,
         created = df$created,
         week_number = cut(created,
                           breaks = c(week_bins, Inf),
                           labels = c(1:length(week_bins)))) %>% 
    unnest_tokens(word, text) %>% 
    filter(!word %in% my_stopwords)
}

popular_text_date <- clean_text_no_tm_2(top_10_percent)

View(popular_text_date)

# popular_text_date <- popular_text_date %>% 
#   mutate(week_number = cut(created,
#                          breaks = c(seq(as.POSIXct(as.character(min(popular_arrange$created))),
#                                       as.POSIXct(as.character(max(popular_arrange$created))),
#                                       by = "7 days"), Inf),
#                          labels = c(1, 2, 3, 4, 5, 6)))

save(popular_text_date, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_text_date.Rdata")

# load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_text_date.Rdata")
# 
# popular_text <- clean_text_no_tm(top_10_percent$text)
# 
# save(popular_text, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_text.Rdata")

unpopular_text_date <- clean_text_no_tm_2(bottom_90_percent)

save(unpopular_text_date, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/unpopular_text_date.Rdata")

# unpopular_text <- clean_text_no_tm(bottom_90_percent$text)
# 
# save(unpopular_text, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/unpopular_text.Rdata")


make_wordcloud <- function(df) {
  freq_df <- df %>%
    filter(word != "false") %>% 
    count(word, sort = TRUE)
  par(mar = rep(0, 4))
  wordcloud(freq_df$word,
            freq_df$n,
            max.words = 60,
            scale = c(5.5, .1),
            random.order = F,
            use.r.layout = T,
            colors = brewer.pal(6, "Dark2"))
} 

popular_text_date %>% 
  filter(week_number == 1) %>% 
  #View()
  make_wordcloud()

make_wordcloud(unpopular_text)

test_weeks <- seq(as.POSIXct('2018-01-27 19:39:19'), as.POSIXct('2018-03-06 16:00:12'),
                  by='7 days')
    

test_4 <- popular_text %>% 
  filter(word != "false") %>% 
  count(word, sort = TRUE) %>%
  # wordcloud(words = word, freq = .$n, max.words = 15, scale = c(5, 0.1), colors = brewer.pal(6, "Dark2"))


wordcloud(test_4$word, test_4$n, max.words = 15, scale = c(5, .1), random.order = F, colors = brewer.pal(6, "Dark2"))


#What are the most common sentiments across tweets? Use nrc sentiment set (assign to emotion group)
sentiments_popular <- popular_text %>%
  right_join(get_sentiments("nrc")) %>%
  #filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

ggplot(sentiments_popular, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity")

sentiments_unpopular <- unpopular_text %>%
  right_join(get_sentiments("nrc")) %>%
  #filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

ggplot(sentiments_unpopular, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity")



#What is the most common word with given sentiment? Use bing sentiment set (binary pos or neg)
# common_words_popular <- popular_text %>%
#   inner_join(get_sentiments("bing")) %>%
#   filter(word != "false") %>% 
#   count(word, sentiment, sort = TRUE) %>%
#   ungroup() %>% 
#   mutate(percent = n/sum(n))
# 
# ggplot(head(common_words_popular, 10), aes(x = word, y = percent, fill = sentiment)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(limits = c(0, 0.035)) +
#   theme_bw()

top_popular_words_sentiment <- popular_text_date %>% 
  inner_join(get_sentiments("bing")) %>% 
  filter(word != "false") %>% 
  #filter(week_number == 6) %>%
  count(word, sentiment, sort = TRUE) %>% 
  #View()
  mutate(percent = n/sum(n)) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(word, -percent), y = percent, fill = sentiment)) +
    geom_bar(stat = "identity") +
    #scale_y_continuous(limits = c(0, 0.03)) +
    labs(
      y = NULL,
      x = NULL,
      fill = NULL
    ) +
  theme(legend.position = "none") +
  theme_void(base_size = 30)

top_popular_words_sentiment
  
make_sentiment_graph <- function(df) {
  df %>% 
    inner_join(get_sentiments("bing")) %>% 
    filter(word != "false") %>% 
    count(word, sentiment, sort = TRUE) %>% 
    mutate(percent = n/sum(n)) %>% 
    head(10) %>% 
    ggplot(aes(x = word, y = percent, fill = sentiment)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 0.03)) +
      theme_bw()
}

make_sentiment_graph(popular_text)
make_sentiment_graph(unpopular_text)

save(top_popular_words_sentiment, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/top_popular_words_sentiment.Rdata")


# common_words_unpopular <- unpopular_text %>%
#   inner_join(get_sentiments("bing")) %>%
#   filter(word != "false") %>% 
#   count(word, sentiment, sort = TRUE) %>%
#   ungroup() %>% 
#   mutate(percent = n/sum(n))
# 
# ggplot(head(common_words_unpopular, 10), aes(x = word, y = percent, fill = sentiment)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(limits = c(0, 0.035)) +
#   theme_bw()

top_unpopular_words_sentiment <- unpopular_text_date %>% 
  inner_join(get_sentiments("bing")) %>% 
  filter(word != "false") %>% 
  filter(week_number == 6) %>%
  count(word, sentiment, sort = TRUE) %>% 
  mutate(percent = n/sum(n)) %>% 
  head(10) %>% 
  ggplot(aes(x = word, y = percent, fill = sentiment)) +
    geom_bar(stat = "identity") +
    #scale_y_continuous(limits = c(0, 0.03)) +
    labs(
      y = "Relative Frequency (word count/total words)",
      x = "Word (alphabetical order)",
      fill = "Sentiment"
    ) +
    theme_minimal(base_size = 12)

top_unpopular_words_sentiment

save(top_unpopular_words_sentiment, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/top_unpopular_words_sentiment.Rdata")


#WORD CORRELATION
# popular_word_cor <- popular_text %>%
#   group_by(word) %>%
#   filter(n() >= 5) %>%
#   pairwise_cor(word, tweet_number, method = "pearson") %>%
#   filter(!is.na(correlation),
#          correlation > .4)

popular_bigram <- popular_text_date %>%
  group_by(word) %>%
  filter(week_number == 1) %>% 
  filter(n() >= 3) %>%
  pairwise_cor(word, tweet_number, method = "pearson") %>%
  filter(!is.na(correlation),
         correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  #geom_node_text(aes(label = name), repel = TRUE, size = 6.5) +
  theme_void()

popular_bigram

make_bigram_graph <- function(df, min_word_number) {
  df %>% 
    group_by(word) %>% 
    filter(n >= min_word_number) %>% 
    pairwise_cor(word, tweet_number, method = "pearson") %>% 
    filter(!is.na(correlation),
           correlation > 0.4) %>% 
    graph_from_data_frame() %>% 
    ggraph(layouy = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
}

# unpopular_word_cor <- unpopular_text %>%
#   group_by(word) %>%
#   filter(n() >= 15) %>%
#   pairwise_cor(word, tweet_number, method = "pearson") %>%
#   filter(!is.na(correlation),
#          correlation > .4)

unpopular_bigram <- unpopular_text_date %>%
  group_by(word) %>%
  filter(week_number == 6) %>% 
  filter(n() >= 8) %>%
  pairwise_cor(word, tweet_number, method = "pearson") %>%
  filter(!is.na(correlation),
         correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

unpopular_bigram

save(popular_bigram, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_bigram.Rdata")
save(unpopular_bigram, file = "~/Documents/GIT/DS1/NLP-twitter-science-news/data/unpopular_bigram.Rdata")

my_pal <- (rainbow(27))

