
counts = plyr::count(popular_text_date, 'word') %>% 
  arrange(desc(freq)) %>% 
  .[1:round(nrow(.)*.1),] %>% 
  select("freq")

pop_thresh <- round(mean(counts$freq))

counts_2 <- plyr::count(unpopular_text_date, 'word') %>% 
  arrange(desc(freq)) %>% 
  .[1:round(nrow(.)*.1),] %>% 
  select("freq")

unpop_thresh <- round(mean(counts_2$freq))

shinyServer(function(input, output) {
  
  # Render options for Popular Plot area
  
  output$popular_plot <- renderPlot({
    req(input$select_users)
    
    # Set condition when and how Popular Wordcloud will be rendered
    
    if (input$select_visualization == "Wordcloud") {
      wc_df_popular <- popular_text_date
      if (input$select_week != "All") {
        wc_df_popular <- filter(popular_text_date, week_number == input$select_week)
      }
      wc_df_popular <- wc_df_popular %>% 
        filter(screen_name %in% input$select_users) %>% 
        dplyr::count(word, sort = TRUE)
      par(mar = rep(0, 4))
      wordcloud(wc_df_popular$word,
                wc_df_popular$n,
                max.words = 40,
                scale = c(7, .1),
                random.order = F,
                colors = brewer.pal(6, "Dark2"))
    }
    
    # Set condition when Popular Sentiment graph will be rendered
    
    if (input$select_visualization == "Sentiment") {
      popular_sentiment <- popular_text_date %>% 
        inner_join(get_sentiments("bing")) %>% 
        filter(word != "false")
      if (input$select_week != "All") {
        popular_sentiment <- filter(popular_sentiment, week_number == input$select_week)
      }
      popular_sentiment <- popular_sentiment %>% 
        filter(screen_name %in% input$select_users) %>%
        dplyr::count(word, sentiment, sort = TRUE) %>% 
        mutate(percent = n/sum(n)) %>% 
        head(10) %>% 
        ggplot(aes(x = reorder(word, -percent), y = percent, fill = sentiment)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, 0.08)) +
        labs(
          y = "Relative Frequency\n(word count/total words)",
          x = NULL,
          fill = "Sentiment"
        ) +
        theme_minimal(base_size = 30) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(popular_sentiment)
    }
    
    # Set condition when Popular Word Correlation will be rendered
    
    if (input$select_visualization == "Word Correlation") {
      popular_word_corr <- popular_text_date %>% 
        group_by(word)
      if (input$select_week != "All") {
        popular_word_corr <- filter(popular_word_corr, week_number == input$select_week) %>% 
          filter(n() >= 3)
      }
      if (input$select_week == "All") {
        popular_word_corr <- filter(popular_word_corr, n() >= pop_thresh)
      }
      popular_word_corr %>% 
        filter(screen_name %in% input$select_users) %>%
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
  
  # Render options for Unpopular Plot area
  
  output$unpopular_plot <- renderPlot({
    req(input$select_users)
    
    # Set condition when and how Unpopular Wordcloud will be rendered
    
    if (input$select_visualization == "Wordcloud") {
      wc_df_unpopular <- unpopular_text_date
      if (input$select_week != "All") {
        wc_df_unpopular <- filter(wc_df_unpopular, week_number == input$select_week)
      }
      wc_df_unpopular <- wc_df_unpopular %>% 
        filter(screen_name %in% input$select_users) %>%
        dplyr::count(word, sort = TRUE)
      par(mar = rep(0, 4))
      wordcloud(wc_df_unpopular$word,
                wc_df_unpopular$n,
                max.words = 40,
                scale = c(7, .1),
                random.order = F,
                colors = brewer.pal(6, "Dark2"))
    }
    
    # Set condition when and how Unpopular Sentiment graph will be rendered
    
    if (input$select_visualization == "Sentiment") {
      unpopular_sentiment <- unpopular_text_date %>% 
        inner_join(get_sentiments("bing")) %>% 
        filter(word != "false")
      if (input$select_week != "All") {
        unpopular_sentiment <- filter(unpopular_sentiment, week_number == input$select_week)
      }
      unpopular_sentiment <- unpopular_sentiment %>% 
        filter(screen_name %in% input$select_users) %>%
        dplyr::count(word, sentiment, sort = TRUE) %>% 
        mutate(percent = n/sum(n)) %>% 
        head(10) %>% 
        ggplot(aes(x = reorder(word, -percent), y = percent, fill = sentiment)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(limits = c(0, 0.08)) +
        labs(
          y = "Relative Frequency\n(word count/total words)",
          x = NULL,
          fill = "Sentiment"
        ) +
        theme_minimal(base_size = 30) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      return(unpopular_sentiment)
    }
    
    # Set condition when and how Unpopular Word Correlation will be rendered
    
    if (input$select_visualization == "Word Correlation") {
      unpopular_word_corr <- unpopular_text_date %>% 
        group_by(word)
      if (input$select_week != "All") {
        unpopular_word_corr <- filter(unpopular_word_corr, week_number == input$select_week) %>% 
          filter(n() >= 9)
      }
      if (input$select_week == "All") {
        unpopular_word_corr <- filter(unpopular_word_corr, n() >= unpop_thresh)
      }
      unpopular_word_corr %>% 
        filter(screen_name %in% input$select_users) %>%
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
