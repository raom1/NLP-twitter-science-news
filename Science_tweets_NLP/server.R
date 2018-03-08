#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(wordcloud)

load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/popular_text_date.Rdata")
load("~/Documents/GIT/DS1/NLP-twitter-science-news/data/unpopular_text_date.Rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$popular_plot <- renderPlot({
    if (input$select_visualization == "Wordcloud") {
      wc_df_popular <- popular_text_date
      if (input$select_week != "All") {
        wc_df_popular <- filter(popular_text_date, week_number == input$select_week)
      }
      wc_df_popular <- wc_df_popular %>% 
        count(word, sort = TRUE)
      par(mar = rep(0, 4))
      wordcloud(wc_df_popular$word,
                wc_df_popular$n,
                max.words = 27,
                scale = c(4, .1),
                random.order = F,
                colors = brewer.pal(6, "Dark2"))
    }
    if (input$select_visualization == "Sentiment") {
      popular_sentiment <- popular_text_date %>% 
        inner_join(get_sentiments("bing")) %>% 
        filter(word != "false")
      if (input$select_week != "All") {
        popular_sentiment <- filter(popular_sentiment, week_number == input$select_week)
      }
      popular_sentiment <- popular_sentiment %>% 
        count(word, sentiment, sort = TRUE) %>% 
        mutate(percent = n/sum(n)) %>% 
        head(10) %>% 
        ggplot(aes(x = word, y = percent, fill = sentiment)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, 0.08)) +
        labs(
          y = "Relative Frequency\n(word count/total words)",
          x = "Word (alphabetical order)",
          fill = "Sentiment"
        ) +
        theme_minimal(base_size = 24) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(popular_sentiment)
    }
    if (input$select_visualization == "Word Correlation") {
      popular_word_corr <- popular_text_date %>% 
        group_by(word)
      if (input$select_week != "All") {
        popular_word_corr <- filter(popular_word_corr, week_number == input$select_week) %>% 
          filter(n() >= 3)
      }
      if (input$select_week == "All") {
        popular_word_corr <- filter(popular_word_corr, n() >= 7)
      }
      popular_word_corr %>% 
        pairwise_cor(word, tweet_number, method = "pearson") %>%
        filter(!is.na(correlation),
               correlation > .4) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), repel = TRUE, size = 7) +
        theme_void()
    }
  })
  output$unpopular_plot <- renderPlot({
    if (input$select_visualization == "Wordcloud") {
      wc_df_unpopular <- unpopular_text_date
      if (input$select_week != "All") {
        wc_df_unpopular <- filter(wc_df_unpopular, week_number == input$select_week)
      }
      wc_df_unpopular <- wc_df_unpopular %>% 
        count(word, sort = TRUE)
      par(mar = rep(0, 4))
      wordcloud(wc_df_unpopular$word,
                wc_df_unpopular$n,
                max.words = 27,
                scale = c(4, .1),
                random.order = F,
                colors = brewer.pal(6, "Dark2"))
    }
    if (input$select_visualization == "Sentiment") {
      unpopular_sentiment <- unpopular_text_date %>% 
        inner_join(get_sentiments("bing")) %>% 
        filter(word != "false")
      if (input$select_week != "All") {
        unpopular_sentiment <- filter(unpopular_sentiment, week_number == input$select_week)
      }
      unpopular_sentiment <- unpopular_sentiment %>% 
        count(word, sentiment, sort = TRUE) %>% 
        mutate(percent = n/sum(n)) %>% 
        head(10) %>% 
        ggplot(aes(x = word, y = percent, fill = sentiment)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, 0.08)) +
        labs(
          y = "Relative Frequency\n(word count/total words)",
          x = "Word (alphabetical order)",
          fill = "Sentiment"
        ) +
        theme_minimal(base_size = 24) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(unpopular_sentiment)
    }
    if (input$select_visualization == "Word Correlation") {
      unpopular_word_corr <- unpopular_text_date %>% 
        group_by(word)
      if (input$select_week != "All") {
        unpopular_word_corr <- filter(unpopular_word_corr, week_number == input$select_week) %>% 
          filter(n() >= 8)
      }
      if (input$select_week == "All") {
        unpopular_word_corr <- filter(unpopular_word_corr, n() >= 20)
      }
      unpopular_word_corr %>% 
        pairwise_cor(word, tweet_number, method = "pearson") %>%
        filter(!is.na(correlation),
               correlation > .4) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), repel = TRUE, size = 7) +
        theme_void()
    }
  })

})
