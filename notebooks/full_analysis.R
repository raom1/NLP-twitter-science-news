library(tidytext)
library(tidyverse)
library(ggplot2)
library(qdap)
library(tm)
library(igraph)
library(ggraph)
library(widyr)
library(twitteR)
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ROAuth)
library(proxy)
library(mclust)
library(RColorBrewer)
library(wordcloud)

consumer_key <- "<Your_Counsumer_Key>"
consumer_secret <- "<Your_Consumer_Secret>"
access_token <- "<Your_Access_Token>"
access_secret <- "<Your_Access_Secret>"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

load("<Path-to-my_oauth")

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

source("~/Documents/GIT/DS1/NLP-twitter-science-news/clean_text.R")

set.seed(123)

#Load full text data frame
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/user_search_full_022518.Rdata")

tweets_followers <- left_join(user_search_full,
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


wss_unpop <- sapply(1:k.max, 
                  function(k){kmeans(unpopular_mat, k, nstart=10,iter.max = 1000 )$tot.withinss})

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

make_word_cloud <- function(cluster_number, df) {
  center <- filter(df, cluster == cluster_number)
  wordcloud(center$terms, center$freq, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))
}

make_word_cloud(1, elbow_popular)
make_word_cloud(2, elbow_popular)
make_word_cloud(3, elbow_popular)

















#run clean_tweet_text on full data frame. possibly save it.
search_corpus <- clean_tweet_text(user_search_full)

#create matrix of words and tweets
search_tdm <- TermDocumentMatrix(search_corpus)

search_dtm <- DocumentTermMatrix(search_corpus)

#Remove sparse terms to better resolve clusers and speed up.
tdm_matrix <- as.matrix(removeSparseTerms(search_tdm, sparse = 0.99)) #Don't set sparsity too low.

dtm_matrix <- as.matrix(removeSparseTerms(search_dtm, sparse = 0.99))


distMatrix <- dist(scale(tdm_matrix), method = "cosine")

fit <- hclust(distMatrix, method="ward.D")

plot(fit)


#Elbow Method
k.max <- 24

wss <- sapply(1:k.max, 
              function(k){kmeans(dtm_matrix, k, nstart=100,iter.max = 1000 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# 11 clusters

# Bayesian Inference Criterion
d_clust <- Mclust(dtm_matrix, G=1:15, 
                  modelNames = mclust.options("emModelNames"))
#Display score for each model at each cluster number and summary of top 3 model-cluter combos
d_clust$BIC

# 15 clusters

k_BCI <- 11
k_elbow <- 15
kmeans_result_BCI <- kmeans(dtm_matrix, k_BCI)
kmeans_result_elbow <- kmeans(dtm_matrix, k_elbow)

# cluster centers
#round(kmeansResult$centers, digits=3)

for (i in 1:k_BCI) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeans_result_BCI$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
}

for (i in 1:k_elbow) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeans_result_elbow$centers[i,], decreasing=T)
  cat(names(s)[1:5], "\n")
}
################

cluster_word_clouds <- function()

elbow_centers <- data.frame(t(kmeans_result_elbow$centers))

colnames(elbow_centers) <- c(1:k_elbow)

elbow_centers$cluster <- as.numeric(colnames(elbow_centers)[max.col(elbow_centers,ties.method="first")])

elbow_centers$terms <- rownames(elbow_centers)

elbow_centers$freq <- rowSums(as.matrix(tdm_matrix))

get_cluster_words <- function(df, clust_num) {
  df %>% 
    filter(cluster == clust_num)
}

elbow_center_1 <- get_cluster_words(elbow_centers, 1)
elbow_center_2 <- get_cluster_words(elbow_centers, 2)
elbow_center_3 <- get_cluster_words(elbow_centers, 3)
elbow_center_4 <- get_cluster_words(elbow_centers, 4)
elbow_center_5 <- get_cluster_words(elbow_centers, 5)
elbow_center_6 <- get_cluster_words(elbow_centers, 6)
elbow_center_7 <- get_cluster_words(elbow_centers, 7)
elbow_center_8 <- get_cluster_words(elbow_centers, 8)
elbow_center_9 <- get_cluster_words(elbow_centers, 9)
elbow_center_10 <- get_cluster_words(elbow_centers, 10)
elbow_center_11 <- get_cluster_words(elbow_centers, 11)

make_wordcloud <- function(df) {
  wordcloud(df$term, df$freq, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))
}

# Why are the wordclouds so small/so few words? ANSWER ME!

make_wordcloud(elbow_center_1) #disease and research
make_wordcloud(elbow_center_2) #trash can
make_wordcloud(elbow_center_3) #super blue blood moon
make_wordcloud(elbow_center_4) #heavy falcon spacex
make_wordcloud(elbow_center_5)
make_wordcloud(elbow_center_6)
make_wordcloud(elbow_center_7) # Why this no work?
make_wordcloud(elbow_center_8)
make_wordcloud(elbow_center_9)
make_wordcloud(elbow_center_10)
make_wordcloud(elbow_center_11)



center_BCI <- data.frame(t(kmeans_result_BCI$centers))

colnames(center_BCI) <- c(1:k_BCI)

center_BCI$cluster <- as.numeric(colnames(center_BCI)[max.col(center_BCI,ties.method="first")])

center_BCI$terms <- rownames(center_BCI)

center_BCI$freq <- rowSums(as.matrix(tdm_matrix))

BCI_1 <- get_cluster_words(center_BCI, 1)
BCI_2 <- get_cluster_words(center_BCI, 2)
BCI_3 <- get_cluster_words(center_BCI, 3)
BCI_4 <- get_cluster_words(center_BCI, 4)
BCI_5 <- get_cluster_words(center_BCI, 5)
BCI_6 <- get_cluster_words(center_BCI, 6)
BCI_7 <- get_cluster_words(center_BCI, 7)
BCI_8 <- get_cluster_words(center_BCI, 8)
BCI_9 <- get_cluster_words(center_BCI, 9)
BCI_10 <- get_cluster_words(center_BCI, 10)
BCI_11 <- get_cluster_words(center_BCI, 11)
BCI_12 <- get_cluster_words(center_BCI, 12)
BCI_13 <- get_cluster_words(center_BCI, 13)
BCI_14 <- get_cluster_words(center_BCI, 14)
BCI_15 <- get_cluster_words(center_BCI, 15)


make_wordcloud(BCI_1) # Disease
make_wordcloud(BCI_2) # invalid 'cex'
make_wordcloud(BCI_3) # trash can
make_wordcloud(BCI_4) # super blue blood moon
make_wordcloud(BCI_5) # heavy falcon spacex
make_wordcloud(BCI_6) # pictures of space?
make_wordcloud(BCI_7)
make_wordcloud(BCI_8)
make_wordcloud(BCI_9)
make_wordcloud(BCI_10)
make_wordcloud(BCI_11)
make_wordcloud(BCI_12) # invalid 'cex'
make_wordcloud(BCI_13) # invalid 'cex'
make_wordcloud(BCI_14) # invalid 'cex'
make_wordcloud(BCI_15) # invalid 'cex'



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


full_text <- clean_text_no_tm(user_search_full$text)

#What are the most common sentiments across tweets? Use nrc sentiment set (assign to emotion group)
sentiments <- full_text %>%
  right_join(get_sentiments("nrc")) %>%
  #filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

ggplot(sentiments, aes(x = sentiment, y = n)) +
  geom_bar(stat = "identity")

#What is the most common word with given sentiment? Use bing sentiment set (binary pos or neg)
common_words <- full_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

ggplot(head(common_words, 10), aes(x = word, y = n, fill = sentiment)) +
  geom_bar(stat = "identity")

#How does the sentiment of tweets change overtime? Tweets are in chronological order already.
#Use afinn sentiment set (scale of -5 to 5 of how positive it is)
full_text %>%
  group_by(tweet_number) %>% 
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(score)) %>% 
  ggplot(aes(x = tweet_number, y = sentiment)) +
  geom_bar(stat = "identity")


#WORD CORRELATION
word_cor <- full_text %>%
  group_by(word) %>%
  filter(n() >= 15) %>%
  pairwise_cor(word, tweet_number, method = "pearson") %>%
  filter(!is.na(correlation),
         correlation > .4)

word_cor %>%
  arrange(desc(correlation))

word_cor %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

