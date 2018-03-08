#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(style = "background-color: #FFF8ED;",
  fluidRow(
    column(12,
           h3("Examining the Popularity of Science Topics through Twitter Data Analysis"),
           fluidRow(
             column(6,
                    selectInput(inputId = "select_visualization",
                                label = "Select Visualization",
                                choices = list("Wordcloud", "Sentiment", "Word Correlation"),
                                selected = "Wordcloud")),
             column(6,
                    radioButtons(inputId = "select_week",
                                 label = "Select Week",
                                 choices = c("All", 1, 2, 3, 4, 5, 6),
                                 selected = "All",
                                 inline = T))
           ),
           fluidRow(
             column(6,
                    h4("Popular Tweets"),
                    plotOutput("popular_plot"),
                    style='padding:2px;height:100%;'
             ),
             column(6,
                    h4("Unpopular Tweets"),
                    plotOutput("unpopular_plot"),
                    style='padding:2px;height:100%;'
             )
           )))))
