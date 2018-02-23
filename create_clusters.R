library(tm)
library(RColorBrewer)
library(wordcloud)
library(cluster)
library(biclust)
library(igraph)
library(fpc)
library(ggplot2)
library(proxy)
library(mclust)
library(tidyverse)

#load cleaned tweets
load("~/Documents/Data Science Bootcamp/R/Twitter_R/search_corpus.Rdata")

#create matrix of words and tweets
search_tdm <- TermDocumentMatrix(search_corpus)

#check it out
inspect(search_tdm)

#What words appear more than 50 times
findFreqTerms(search_tdm, 50)

#What words are associated with selected word more than 30% of the time
findAssocs(search_tdm, "launch", 0.3)

#New matrix with axes flipped
search_dtm <- DocumentTermMatrix(search_corpus)

#check it out
inspect(search_dtm)

#TDM and DTM are simplified matricies. Convert to a full matrix for downstream processing.
freq <- colSums(as.matrix(search_dtm))

set.seed(142) #For reproducibility
#One kind of word cloud
wordcloud(names(freq), freq, min.freq=30, rot.per = 0, fixed.asp = F, ordered.colors = T, scale = c(5, .00001))   
#A diferent kind of wordcloud
wordcloud(names(freq), freq, min.freq=30, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

#ARE THERE GROUPS THAT THE TWEETS FALL INTO? Use clustering to answer this question.
#Clustering is harder (and more time consuming) with many infrequently used words.
#Remove sparse terms to better resolve clusers and speed up.
search_tdm_sparse <- removeSparseTerms(search_tdm, sparse=0.99) #Don't set sparsity too low.

m2 <- as.matrix(search_tdm_sparse)

#Calculate distance between each word. HOW DOES THIS WORK???
distMatrix <- dist(scale(m2), method = "cosine")

#Clustering words based on distance. Will result in dendrogram.
#Words that occur together more often (lower distance) will cluster together.
fit <- hclust(distMatrix, method="ward.D")

#Visualize the dendrogram.
plot(fit)

#Computer will draw rectangles around groups. Only looking at height of dendrogram branch.
#If rerun with different k arg then rectangles are overlaid. Replot to remove.
rect.hclust(fit, k=6)

#####################
#This code chunk (next 9 lines) produces a different result from the above chunk. Why?
#its the transpose (t) in 3rd line. remove that part and it looks like above. 
#Why is it done here?
search_tdms <- removeSparseTerms(search_tdm, 0.99)
search_mat <- as.matrix(search_tdms)
d <- dist(t(search_mat), method="cosine")   
fit <- hclust(d=d, method="ward.D")   # for a different look try substituting: method="ward.D"
plot(fit, hang=-1)

rect.hclust(fit,6)

kfit <- kmeans(dist(freq), 6, nstart=100)

#transpose the matrix to cluster documents (tweets)
m3 <- t(m2)

####HOW TO DETERMINE THE NUMBER OF CLUSTERS? Two methods...####

################
#Elbow Method for finding the optimal number of clusters
#This method will group points into a number of clusters set by k.max.
#It will then calculate how far each point is from the center of the cluster (withinss/wss)
#The results will be plot with number of clusters on the x and wss on the y
#The goal is to find a value of x where the y value for that point and the next are roughly equal.
#Said another way you want to find two points where the value of y are roughly equal and take the lower x value.
#This looks like an elbow (kinda).
set.seed(123)
#try up to 24 clusters
k.max <- 24
#setting what data to look at.
data <- m3
#Use sapply to run k means on each number of clusters. Save total within sum of squars to variable wss.
#This will take a while to run because the clustering will be allowed to run for a long time.
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=100,iter.max = 1000 )$tot.withinss})
#Display total sum of squares
wss
#plot to observe elbow
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#Multiple "Elbows" appear to be present (around 3 at 4, 9, and 21).
#I will take the first observed, 4
################

################
#Bayesian inference criterion (BIC) for k means
#This method tries multiple different models on the data.
#Models vary based on how they assume the data are distributed.
#Each model tries a range of cluster numbers and calculates a score for each method along with
#an optimal number of clusters.

#Trying range of 1 to 15 clusters using the models found in "emModelNames"
#It will take time to fit the data to each model. A progress bar will show in the console.
#The more clusters you try to longer the run time.
d_clust <- Mclust(m3, G=1:15, 
                  modelNames = mclust.options("emModelNames"))
#Display score for each model at each cluster number and summary of top 3 model-cluter combos
d_clust$BIC
#Plot results for each model with number of clusters on x and score on y
plot(d_clust$BIC)
#6 looks like the best using EEI model

#Summary stats. Includes number of rows contained in each cluster
summary(d_clust)

#Too many dimensions to plot, can't do classification, density, uncertainty plot
#plot(d_clust, what = "classification")
################

######################
#Now calculated number of clusters using two methods. 
#They didn't agree but maybe making parameters more similar may help in the future.
#Try calculating k means with both 4 and 6 for k.

#NOT SURE WHY DO THIS, FIGURE THIS OUT. It flips rows and cols matrix
# set a fixed random seed
set.seed(100)
# k-means clustering of tweets
k_BCI <- 6
k_elbow <- 4
kmeans_result_BCI <- kmeans(m3, k_BCI)
kmeans_result_elbow <- kmeans(m3, k_elbow)

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


center_df <- data.frame(t(kmeans_result_elbow$centers))

colnames(center_df) <- c(1, 2, 3, 4)

cluster_num <- as.numeric(colnames(center_df)[max.col(center_df,ties.method="first")])

center_df$cluster <- as.numeric(cluster_num)

center_df$terms <- rownames(center_df)

freq_tdm <- rowSums(as.matrix(search_tdm_sparse))
center_df$freq <- freq_tdm

get_cluster_words <- function(df, clust_num) {
  df %>% 
    filter(cluster == clust_num)
}

center_1 <- get_cluster_words(center_df, 1)

center_2 <- get_cluster_words(center_df, 2)

center_3 <- get_cluster_words(center_df, 3)

center_4 <- get_cluster_words(center_df, 4)

make_wordcloud <- function(df) {
  wordcloud(df$term, df$freq, scale = c(5, .1), colors = brewer.pal(6, "Dark2"))
}

make_wordcloud(center_1) #disease and research

make_wordcloud(center_2) #trash can

make_wordcloud(center_3) #super blue blood moon

make_wordcloud(center_4) #heavy falcon spacex



center_BCI <- data.frame(t(kmeans_result_BCI$centers))

colnames(center_BCI) <- c(1:k_BCI)

center_BCI$cluster <- as.numeric(colnames(center_BCI)[max.col(center_BCI,ties.method="first")])

center_BCI$terms <- rownames(center_BCI)

center_BCI$freq <- rowSums(as.matrix(search_tdm_sparse))

BCI_1 <- get_cluster_words(center_BCI, 1)

BCI_2 <- get_cluster_words(center_BCI, 2)

BCI_3 <- get_cluster_words(center_BCI, 3)

BCI_4 <- get_cluster_words(center_BCI, 4)

BCI_5 <- get_cluster_words(center_BCI, 5)

BCI_6 <- get_cluster_words(center_BCI, 6)


make_wordcloud(BCI_1) #Disease
make_wordcloud(BCI_2) #Not sure, something about plants?
make_wordcloud(BCI_3) #trash can
make_wordcloud(BCI_4) #super blue blood moon
make_wordcloud(BCI_5) #heavy falcon spacex
make_wordcloud(BCI_6) #pictures of space?


################
#Another method for determining number of clusters

# install.packages("NbClust",dependencies = TRUE)
# library(NbClust)
# nb <- NbClust(scaled_data, diss=NULL, distance = "euclidean", 
#               min.nc=2, max.nc=5, method = "kmeans", 
#               index = "all", alphaBeale = 0.1)
# hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
################

#https://en.m.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
# In text databases, a document collection defined by a document by term D matrix 
# (of size m by n, m: number of documents, n: number of terms) number of clusters can 
# roughly be estimated by the following formula mn/t where t is the number of non-zero 
# entries in D. Note that in D each row and each column must contain at least one non-zero 
# element