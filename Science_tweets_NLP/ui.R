#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinythemes::shinytheme("superhero"),
  fluidRow(
    column(12,
           h1("Examining the Popularity of Science Topics through Twitter Data Analysis"),
  fluidRow(
             column(6,
                    selectInput(inputId = "select_visualization",
                                label = h3("Select Visualization"),
                                choices = list("Wordcloud", "Sentiment", "Word Correlation"),
                                selected = "Wordcloud")),
             column(6,
                    radioButtons(inputId = "select_week",
                                 label = h3("Select Week"),
                                 choices = c("All", 1, 2, 3, 4, 5, 6),
                                 selected = "All",
                                 inline = T))
           ),
  fluidRow(
             column(6,
                    div(style = "height:750px;",
                    h2("Popular Tweets"),
                    plotOutput("popular_plot", width = "100%", height = "95%")
                    )
             ),
             column(6,
                    div(style = "height:750px;",
                    h2("Unpopular Tweets"),
                    plotOutput("unpopular_plot", width = "100%", height = "95%")
                    )
             )
           )))))


